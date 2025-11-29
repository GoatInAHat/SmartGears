#!/usr/bin/env python3
"""
Build an offline, navigable copy of the AutoLISP help docs using the
navigation HTML snippet stored at "AutoLISP docs/AutoLISPdoclisthtml.txt".

The script builds a folder hierarchy under docs-autolisp/ that mirrors the nav
structure, retrieves each page, captures referenced assets, rewrites internal
links, and now supports Markdown output that is easier for autonomous agents to
parse.

Usage:
    python3 scripts/build_offline_autolisp_docs.py [--force] [--limit N] \
        [--format {markdown,html}]

Flags:
    --force      Redownload pages even if the target file already exists and
                 purge stale files from previous formats.
    --limit      Download only the first N pages (useful for debugging).
    --format     Choose the final file format. Markdown is the default because
                 it is lighter-weight for tooling while staying fully offline.
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
from bs4 import BeautifulSoup, NavigableString, Tag

NAV_FILE = Path("AutoLISP docs/AutoLISPdoclisthtml.txt")
DOC_ROOT = Path("docs-autolisp")
ASSET_DIR = DOC_ROOT / "_assets"
NAVMAP_JSON = DOC_ROOT / "_navmap.json"
BASE_HOST = "https://help.autodesk.com"
BASE_VIEW = "https://help.autodesk.com/view/OARX/2025/ENU/"
OUTPUT_FORMATS = {"html": ".html", "markdown": ".md"}
USER_AGENT = {
    "User-Agent": "SmartGears-doc-downloader/2.0 (+https://github.com/bennett/smartgears)"
}
INLINE_PUNCTUATION = set(".,:;!?)]}>'\"")


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


def purge_with_extension(ext: str) -> None:
    if not DOC_ROOT.exists():
        return
    pattern = f"*{ext}"
    for path in DOC_ROOT.rglob(pattern):
        if "_assets" in path.parts:
            continue
        if path.is_file():
            path.unlink(missing_ok=True)


def build_mapping(root: NavNode, output_format: str):
    pages = []
    url_map: Dict[str, Path] = {}
    ext = OUTPUT_FORMATS[output_format]

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
            file_path = file_dir / f"{segment}{ext}"
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
    for img in list(soup.find_all("img")):
        src = img.get("src")
        if not src:
            continue
        resolved = urljoin(page_url, src)
        local = fetch_asset(resolved, asset_cache)
        if local:
            img["src"] = os.path.relpath(local, target_dir)
    for script in list(soup.find_all("script")):
        if script.get("src"):
            script.decompose()


def normalize_space(text: str) -> str:
    return re.sub(r"\s+", " ", text)


def needs_space(prev: str, curr: str) -> bool:
    if not prev or prev.endswith((" ", "\n", "(", "/")):
        return False
    if not curr:
        return False
    return curr[0] not in INLINE_PUNCTUATION


def inline_text(node) -> str:
    if isinstance(node, NavigableString):
        return normalize_space(str(node))
    if not isinstance(node, Tag):
        return ""
    name = node.name.lower()
    if name in {"strong", "b"}:
        inner = inline_text_join(node)
        return f"**{inner}**" if inner else ""
    if name in {"em", "i"}:
        inner = inline_text_join(node)
        return f"*{inner}*" if inner else ""
    if name in {"code", "samp", "kbd", "tt"}:
        inner = inline_text_join(node)
        if not inner:
            return ""
        fence = "`"
        while fence in inner:
            fence += "`"
        return f"{fence}{inner}{fence}"
    if name == "span":
        return inline_text_join(node)
    if name == "a":
        href = (node.get("href") or "").strip()
        inner = inline_text_join(node) or href
        if href:
            return f"[{inner}]({href})"
        return inner
    if name == "br":
        return "  \n"
    if name == "img":
        alt = (node.get("alt") or "").strip()
        src = (node.get("src") or "").strip()
        if not src:
            return alt
        return f"![{alt}]({src})"
    if name in {"sup", "sub"}:
        inner = inline_text_join(node)
        wrapper = "^" if name == "sup" else "~"
        return f"{wrapper}{inner}{wrapper}" if inner else ""
    return inline_text_join(node)


def inline_text_join(node) -> str:
    parts: List[str] = []
    for child in node.children:
        fragment = inline_text(child)
        if not fragment:
            continue
        if parts and needs_space(parts[-1], fragment):
            parts.append(" ")
        parts.append(fragment)
    return "".join(parts).strip()


class MarkdownBuilder:
    def __init__(self):
        self.lines: List[str] = []

    def add_blank(self):
        if self.lines and self.lines[-1] == "":
            return
        self.lines.append("")

    def add_line(self, line: str, indent: int = 0):
        if line is None:
            self.lines.append("")
            return
        prefix = " " * max(indent, 0)
        trimmed = line.rstrip()
        self.lines.append(f"{prefix}{trimmed}" if trimmed else "")

    def extend(self, lines: List[str]):
        for line in lines:
            self.add_line(line)

    def render(self) -> str:
        text = "\n".join(self.lines)
        text = re.sub(r"\n{3,}", "\n\n", text).strip()
        return text + ("\n" if text else "")


def render_table(table: Tag) -> List[str]:
    all_rows = table.find_all("tr")
    if not all_rows:
        return []
    header_cells: List[str] = []
    body_rows: List[List[str]] = []
    thead = table.find("thead")
    header_row = None
    if thead:
        header_row = thead.find("tr")
    if not header_row and all_rows and all_rows[0].find("th"):
        header_row = all_rows[0]
    for tr in all_rows:
        cells = [inline_text(cell).strip() for cell in tr.find_all(["th", "td"], recursive=False)]
        if not cells:
            continue
        if tr is header_row:
            header_cells = cells
        else:
            body_rows.append(cells)
    width = max([len(header_cells)] + [len(row) for row in body_rows]) if (header_cells or body_rows) else 0
    if width == 0:
        return []
    def pad(row: List[str]) -> List[str]:
        return row + [""] * (width - len(row))
    header = pad(header_cells or [""] * width)
    lines = ["| " + " | ".join(header) + " |"]
    lines.append("| " + " | ".join(["---"] * width) + " |")
    for row in body_rows:
        padded = pad(row)
        lines.append("| " + " | ".join(padded) + " |")
    return lines


def render_list(list_tag: Tag, builder: MarkdownBuilder, indent: int, ordered: bool):
    items = list_tag.find_all("li", recursive=False)
    if not items:
        return
    for idx, li in enumerate(items, 1):
        prefix = f"{idx}." if ordered else "-"
        li_builder = MarkdownBuilder()
        for child in li.children:
            render_block(child, li_builder, indent=0)
        li_lines = [line for line in li_builder.lines if line is not None]
        while li_lines and li_lines[0] == "":
            li_lines.pop(0)
        while li_lines and li_lines[-1] == "":
            li_lines.pop()
        if not li_lines:
            continue
        builder.add_line(f"{' ' * indent}{prefix} {li_lines[0]}")
        continuation_indent = indent + len(prefix) + 1
        for line in li_lines[1:]:
            if line == "":
                builder.add_line("")
            else:
                builder.add_line(line, indent=continuation_indent)
    builder.add_blank()


def render_block(node, builder: MarkdownBuilder, indent: int = 0):
    if isinstance(node, NavigableString):
        text = normalize_space(str(node))
        if text:
            builder.add_line(text, indent=indent)
        return
    if not isinstance(node, Tag):
        return
    name = node.name.lower()
    if name in {"script", "style", "noscript", "iframe"}:
        return
    if name in {"p", "shortdesc"}:
        text = inline_text(node)
        if text:
            builder.add_blank()
            builder.add_line(text, indent=indent)
            builder.add_blank()
        return
    if name in {"h1", "h2", "h3", "h4", "h5", "h6"}:
        level = int(name[1])
        text = inline_text(node).strip()
        if text:
            builder.add_blank()
            builder.add_line("#" * level + " " + text)
            builder.add_blank()
        return
    if name == "pre":
        code_text = node.get_text("\n", strip=False).strip("\n")
        builder.add_blank()
        builder.add_line("```lisp")
        for line in code_text.splitlines():
            builder.add_line(line.rstrip())
        builder.add_line("```")
        builder.add_blank()
        return
    if name == "ul":
        render_list(node, builder, indent, ordered=False)
        return
    if name == "ol":
        render_list(node, builder, indent, ordered=True)
        return
    if name == "table":
        lines = render_table(node)
        if lines:
            builder.add_blank()
            builder.extend(lines)
            builder.add_blank()
        return
    if name == "blockquote":
        child_builder = MarkdownBuilder()
        for child in node.children:
            render_block(child, child_builder)
        for line in child_builder.lines:
            if line == "":
                builder.add_line(">")
            else:
                builder.add_line(f"> {line}")
        builder.add_blank()
        return
    if name == "hr":
        builder.add_blank()
        builder.add_line("---")
        builder.add_blank()
        return
    if name == "dl":
        current_term = None
        for child in node.children:
            if not isinstance(child, Tag):
                continue
            if child.name == "dt":
                current_term = inline_text(child).strip()
            elif child.name == "dd" and current_term:
                definition = inline_text(child).strip()
                builder.add_line(f"- **{current_term}:** {definition}")
        builder.add_blank()
        return
    for child in node.children:
        render_block(child, builder, indent=indent)


def find_content_root(soup: BeautifulSoup) -> Tag:
    selectors = [
        "div.body.conbody",
        "div.conbody",
        "article",
        "main",
        "div.topic",
        "body",
    ]
    for selector in selectors:
        candidate = soup.select_one(selector)
        if candidate:
            return candidate
    return soup


def strip_short_description(soup: BeautifulSoup) -> Optional[str]:
    shortdesc = soup.find("p", class_="shortdesc")
    if not shortdesc:
        return None
    text = shortdesc.get_text(" ", strip=True)
    shortdesc.decompose()
    return text


def prune_auxiliary_content(soup: BeautifulSoup) -> None:
    selectors = [
        "div.head",
        "div.ancestry",
        "div.button-info",
        "div.feedback",
        "div.related-links",
        "div.topicToolbar",
        "div.footer",
        "nav",
        "header",
        "footer",
    ]
    for selector in selectors:
        for tag in soup.select(selector):
            tag.decompose()
    for unwanted in soup.find_all(["script", "style", "noscript", "iframe"]):
        unwanted.decompose()


def collect_metadata(soup: BeautifulSoup, entry: Dict[str, str]) -> Dict[str, object]:
    meta_map: Dict[str, List[str]] = {}
    for meta in soup.find_all("meta"):
        name = (meta.get("name") or "").strip().lower()
        if not name:
            continue
        content = (meta.get("content") or "").strip()
        if not content:
            continue
        meta_map.setdefault(name, []).append(content)
    title = soup.title.get_text(" ", strip=True) if soup.title else entry["title"]
    def pick(key: str) -> Optional[str]:
        values = meta_map.get(key, [])
        return values[0] if values else None
    return {
        "title": title,
        "guid": entry.get("guid") or pick("topicid"),
        "source_url": entry["url"],
        "generated": datetime.utcnow().isoformat() + "Z",
        "description": pick("description"),
        "topic_type": pick("topic-type"),
        "topic_subtype": meta_map.get("topic-subtype", []),
        "audience": pick("audience"),
        "experience_level": pick("experiencelevel"),
        "product": pick("product"),
        "release": pick("release"),
        "book": pick("book"),
        "component": pick("component"),
        "tags": meta_map.get("indexterm", []),
        "created": pick("created"),
        "modified": pick("modified"),
    }


def yaml_escape(value: str) -> str:
    if value is None:
        return """"""
    text = str(value)
    if not text:
        return """"""
    if re.search(r"[:#\-{}\[\]\n\t]", text) or text.strip() != text:
        return json.dumps(text, ensure_ascii=False)
    return text


def format_front_matter(metadata: Dict[str, object]) -> str:
    lines = ["---"]
    scalar_keys = [
        "title",
        "guid",
        "source_url",
        "generated",
        "description",
        "topic_type",
        "audience",
        "experience_level",
        "product",
        "release",
        "book",
        "component",
        "created",
        "modified",
    ]
    for key in scalar_keys:
        value = metadata.get(key)
        if value:
            lines.append(f"{key}: {yaml_escape(value)}")
    list_keys = {"topic_subtype": "topic_subtype", "tags": "tags"}
    for key, label in list_keys.items():
        values = metadata.get(key) or []
        if not values:
            continue
        lines.append(f"{label}:")
        for item in values:
            lines.append(f"  - {yaml_escape(item)}")
    lines.append("---")
    return "\n".join(lines)


def build_quick_reference(metadata: Dict[str, object]) -> List[str]:
    items: List[str] = []
    source = metadata.get("source_url")
    if source:
        items.append(f"- Source: [{source}]({source})")
    if metadata.get("topic_type"):
        items.append(f"- Topic Type: {metadata['topic_type']}")
    subtypes = metadata.get("topic_subtype") or []
    if subtypes:
        items.append(f"- Subtypes: {', '.join(subtypes)}")
    if metadata.get("audience"):
        items.append(f"- Audience: {metadata['audience']}")
    if metadata.get("experience_level"):
        items.append(f"- Experience Level: {metadata['experience_level']}")
    if metadata.get("product") or metadata.get("release"):
        parts = [part for part in [metadata.get("product"), metadata.get("release")] if part]
        items.append(f"- Applies To: {' / '.join(parts)}")
    if metadata.get("book") or metadata.get("component"):
        parts = [part for part in [metadata.get("book"), metadata.get("component")] if part]
        items.append(f"- Collection: {' / '.join(parts)}")
    if metadata.get("created") or metadata.get("modified"):
        parts = []
        if metadata.get("created"):
            parts.append(f"Created {metadata['created']}")
        if metadata.get("modified"):
            parts.append(f"Updated {metadata['modified']}")
        items.append(f"- Timeline: {', '.join(parts)}")
    tags = metadata.get("tags") or []
    if tags:
        items.append(f"- Keywords: {', '.join(tags)}")
    return items


def convert_to_markdown(soup: BeautifulSoup, entry: Dict[str, str]) -> str:
    prune_auxiliary_content(soup)
    shortdesc = strip_short_description(soup)
    metadata = collect_metadata(soup, entry)
    title = metadata.get("title") or entry["title"]
    content_root = find_content_root(soup)
    builder = MarkdownBuilder()
    for child in content_root.children:
        render_block(child, builder)
    content = builder.render().strip()
    parts: List[str] = []
    front_matter = format_front_matter(metadata)
    parts.append(front_matter)
    parts.append("")
    parts.append(f"# {title}")
    parts.append("")
    if shortdesc:
        parts.append(f"> {shortdesc}")
        parts.append("")
    quick = build_quick_reference(metadata)
    if quick:
        parts.append("## Quick Reference")
        parts.append("")
        parts.extend(quick)
        parts.append("")
    if content:
        parts.append(content)
    parts.append("")
    return "\n".join(parts).strip() + "\n"


def download_page(entry, url_map, asset_cache, output_format, force=False):
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
    if output_format == "html":
        target.write_text(soup.prettify(), encoding="utf-8")
    else:
        markdown = convert_to_markdown(soup, entry)
        target.write_text(markdown, encoding="utf-8")


def main(argv: List[str]):
    parser = argparse.ArgumentParser(description="Download offline AutoLISP help docs")
    parser.add_argument("--force", action="store_true", help="Redownload pages even if present")
    parser.add_argument("--limit", type=int, default=None, help="Limit number of pages (debugging)")
    parser.add_argument(
        "--format",
        choices=sorted(OUTPUT_FORMATS.keys()),
        default="markdown",
        help="Choose file format for the generated pages",
    )
    args = parser.parse_args(argv)

    if not NAV_FILE.exists():
        print(f"Navigation file missing: {NAV_FILE}")
        sys.exit(1)

    soup = BeautifulSoup(NAV_FILE.read_text(encoding="utf-8"), "html.parser")
    root = build_tree(soup)

    ensure_dir(DOC_ROOT)
    ensure_dir(ASSET_DIR)

    if args.force:
        other_ext = ".html" if args.format == "markdown" else ".md"
        purge_with_extension(other_ext)

    pages, url_map = build_mapping(root, args.format)

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
        download_page(entry, url_map, asset_cache, args.format, force=args.force)

    print("Done. Offline docs live under docs-autolisp/ in", args.format.upper(), "format.")


if __name__ == "__main__":
    main(sys.argv[1:])
