import pathlib
import re
import unittest


class PromptInitgetTests(unittest.TestCase):
    def test_prompts_avoid_initget(self):
        """Interactive prompts should not rely on initget to avoid 'too few arguments' crashes."""

        text = pathlib.Path("src/sg-load.lsp").read_text(encoding="utf-8")
        initget_calls = list(re.finditer(r"\(initget\s", text))

        self.assertEqual(
            initget_calls,
            [],
            "Interactive gear prompts must not call initget; AutoCAD reports 'too few arguments' when defaults are accepted.",
        )


if __name__ == "__main__":
    unittest.main()
