#!/usr/bin/env python3
"""
Build an offline, navigable copy of the AutoLISP help docs using the
navigation HTML snippet stored at "AutoLISP docs/AutoLISPdoclisthtml.txt".

The script creates a hierarchy under docs-autolisp/ that mirrors the nav tree,
retrieves each page, rewrites internal links to point to the local copies, and
pulls down referenced images and stylesheets into a shared _assets folder.

Usage:
    python3 scripts/build_offline_autolisp_docs.py [--force] [--limit N]

Flags:
    --force   Redownload pages even if the target file already exists.
    --limit   Download only the first N pages (useful for debugging).
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import sys
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional
from urllib.parse import urljoin, urlparse, urlunparse

import requests
from bs4 import BeautifulSoup

NAV_FILE = Path("AutoLISP docs/AutoLISPdoclisthtml.txt")
DOC_ROOT = Path("docs-autolisp")
ASSET_DIR = DOC_ROOT / "_assets"
NAVMAP_JSON = DOC_ROOT / "_navmap.json"
BASE_HOST = "https://help.autodesk.com"
BASE_VIEW = "https://help.autodesk.com/view/OARX/2025/ENU/"

USER_AGENT = {
    "User-Agent": "SmartGears-doc-downloader/1.0 (+https://github.com/bennett/smartgears)"
}


@dataclass
class NavNode:
    title: str
    url: Optional[str]
    guid: Optional[str]
    children: List["NavNode"] = field(default_factory=list)


def sanitize_segment(text: str) -> str:
    cleaned = text.strip()
    cleaned = cleaned.replace("’", "'").replace("“", "").replace("”", "")
    cleaned = re.sub(r"[^A-Za-z0-9]+", "_", cleaned)
    cleaned = re.sub(r"_+", "_", cleaned).strip("_")
    if not cleaned:
        cleaned = "section"
    return cleaned[:80]


def extract_guid(url: str) -> Optional[str]:
    match = re.search(r"(GUID-[A-Za-z0-9-]+)", url or "")
    return match.group(1) if match else None


def canonical_url(url: str) -> str:
    parsed = urlparse(url)
    fragmentless = parsed._replace(fragment="")
    scheme = fragmentless.scheme or "https"
    netloc = fragmentless.netloc or "help.autodesk.com"
    return urlunparse((scheme, netloc, fragmentless.path, fragmentless.params, fragmentless.query, ""))


def parse_li(li) -> NavNode:
    anchor = li.find("a", recursive=False)
    title = anchor.get_text(strip=True) if anchor else (li.get("data-id") or "untitled")
    href = anchor.get("data-url") if anchor else None
    href = href or (anchor.get("href") if anchor else None)
    url = None
    if href and href != "#":
        if href.startswith("?"):
            url = urljoin(BASE_VIEW, href)
        elif href.startswith("/"):
            url = urljoin(BASE_HOST, href)
        else:
            url = urljoin(BASE_HOST, href)
    guid = extract_guid(href or "") if href else None
    children = []
    ul = li.find("ul", recursive=False)
    if ul:
        for child in ul.find_all("li", recursive=False):
            children.append(parse_li(child))
    return NavNode(title=title, url=url, guid=guid, children=children)


def build_tree(soup: BeautifulSoup) -> NavNode:
    root = soup.find("li", {"data-id": "AutoCAD-AutoLISP-Header_id"})
    if not root:
        root = soup.find("li")
    return parse_li(root)


def ensure_dir(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)


def asset_filename(url: str) -> str:
    parsed = urlparse(url)
    name = os.path.basename(parsed.path) or "asset"
    base, ext = os.path.splitext(name)
    base = sanitize_segment(base) or "asset"
    digest = hashlib.sha1(url.encode("utf-8")).hexdigest()[:8]
    return f"{base}-{digest}{ext}"


def fetch_asset(url: str, cache: Dict[str, Path]) -> Optional[Path]:
    key = canonical_url(url)
    if key in cache:
        return cache[key]
    try:
        resp = requests.get(url, headers=USER_AGENT, timeout=30)
        resp.raise_for_status()
    except Exception as exc:  # pragma: no cover - network variability
        print(f"[asset] skip {url} ({exc})")
        return None
    ensure_dir(ASSET_DIR)
    fname = asset_filename(url)
    target = ASSET_DIR / fname
    mode = "wb"
    data = resp.content
    with open(target, mode) as handle:
        handle.write(data)
    cache[key] = target
    return target


def build_mapping(root: NavNode):
    pages = []
    url_map: Dict[str, Path] = {}

    def walk(node: NavNode, ancestors: List[str]):
        segment = sanitize_segment(node.title)
        has_children = bool(node.children)
        child_ancestors = ancestors + [segment]

        if has_children:
            ensure_dir(DOC_ROOT / Path(*child_ancestors))

        if node.url:
            if has_children:
                file_dir = DOC_ROOT / Path(*child_ancestors)
            else:
                file_dir = DOC_ROOT / Path(*ancestors)
            ensure_dir(file_dir)
            file_path = file_dir / f"{segment}.html"
            entry = {
                "title": node.title,
                "url": canonical_url(node.url),
                "guid": node.guid,
                "file_path": file_path,
                "rel_path": file_path.relative_to(DOC_ROOT),
            }
            pages.append(entry)
            url_map[canonical_url(node.url)] = file_path
            if node.guid:
                alt_cloud = f"https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/{node.guid}.htm"
                alt_view = f"https://help.autodesk.com/view/OARX/2025/ENU/?guid={node.guid}"
                url_map[canonical_url(alt_cloud)] = file_path
                url_map[canonical_url(alt_view)] = file_path

        for child in node.children:
            walk(child, child_ancestors)

    walk(root, [])
    return pages, url_map


def rewrite_links(soup: BeautifulSoup, page_url: str, target_dir: Path, url_map: Dict[str, Path]):
    for tag in soup.find_all("a"):
        href = tag.get("href")
        if not href:
            continue
        if href.startswith("javascript"):
            continue
        if href.startswith("#"):
            continue
        if href.startswith("?"):
            resolved = canonical_url(urljoin(BASE_VIEW, href))
        else:
            resolved = canonical_url(urljoin(page_url, href))
        dest = url_map.get(resolved)
        if dest:
            relative = os.path.relpath(dest, target_dir)
            tag["href"] = relative


def rewrite_assets(soup: BeautifulSoup, page_url: str, target_dir: Path, asset_cache: Dict[str, Path]):
    # Stylesheets
    for link in list(soup.find_all("link")):
        rel = link.get("rel") or []
        href = link.get("href")
        if not href:
            continue
        if "stylesheet" in rel:
            resolved = urljoin(page_url, href)
            local = fetch_asset(resolved, asset_cache)
            if local:
                link["href"] = os.path.relpath(local, target_dir)
            else:
                link.decompose()
    # Images
    for img in list(soup.find_all("img")):
        src = img.get("src")
        if not src:
            continue
        resolved = urljoin(page_url, src)
        local = fetch_asset(resolved, asset_cache)
        if local:
            img["src"] = os.path.relpath(local, target_dir)

    # Strip external scripts to keep pages self-contained and lightweight
    for script in list(soup.find_all("script")):
        if script.get("src"):
            script.decompose()


def download_page(entry, url_map, asset_cache, force=False):
    target = entry["file_path"]
    if target.exists() and not force:
        return
    url = entry["url"]
    try:
        resp = requests.get(url, headers=USER_AGENT, timeout=30)
        resp.raise_for_status()
    except Exception as exc:  # pragma: no cover - network variability
        print(f"[page] failed {url}: {exc}")
        return
    soup = BeautifulSoup(resp.text, "html.parser")
    rewrite_assets(soup, resp.url, target.parent, asset_cache)
    rewrite_links(soup, resp.url, target.parent, url_map)
    target.write_text(soup.prettify(), encoding="utf-8")


def main(argv: List[str]):
    parser = argparse.ArgumentParser(description="Download offline AutoLISP help docs")
    parser.add_argument("--force", action="store_true", help="Redownload pages even if present")
    parser.add_argument("--limit", type=int, default=None, help="Limit number of pages (debugging)")
    args = parser.parse_args(argv)

    if not NAV_FILE.exists():
        print(f"Navigation file missing: {NAV_FILE}")
        sys.exit(1)

    soup = BeautifulSoup(NAV_FILE.read_text(encoding="utf-8"), "html.parser")
    root = build_tree(soup)

    ensure_dir(DOC_ROOT)
    ensure_dir(ASSET_DIR)

    pages, url_map = build_mapping(root)

    if args.limit:
        pages = pages[: args.limit]

    navmap = {
        "generated": datetime.utcnow().isoformat() + "Z",
        "pages": [
            {
                "title": p["title"],
                "guid": p["guid"],
                "url": p["url"],
                "rel_path": str(p["rel_path"]),
            }
            for p in pages
        ],
    }
    NAVMAP_JSON.parent.mkdir(parents=True, exist_ok=True)
    NAVMAP_JSON.write_text(json.dumps(navmap, indent=2), encoding="utf-8")

    asset_cache: Dict[str, Path] = {}
    for idx, entry in enumerate(pages, 1):
        print(f"[{idx}/{len(pages)}] {entry['title']}")
        download_page(entry, url_map, asset_cache, force=args.force)

    print("Done. Offline docs live under docs-autolisp/")


if __name__ == "__main__":
    main(sys.argv[1:])
