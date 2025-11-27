import pathlib
import re
import unittest


class PromptInitgetTests(unittest.TestCase):
    def test_initget_always_has_flags(self):
        """Ensure initget is never invoked without a bit-coded flags argument."""
        text = pathlib.Path("src/sg-load.lsp").read_text(encoding="utf-8")
        zero_arg_calls = list(re.finditer(r"\(initget\s*\)", text))
        self.assertEqual(
            zero_arg_calls,
            [],
            "initget must be called with flags; an empty call triggers 'too few arguments' in AutoCAD when accepting defaults.",
        )


if __name__ == "__main__":
    unittest.main()
