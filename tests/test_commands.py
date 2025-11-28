import pathlib
import re
import unittest


class CommandRegistrationTests(unittest.TestCase):
    def setUp(self):
        self.text = pathlib.Path("src/sg-load.lsp").read_text(encoding="utf-8")

    def test_noninteractive_test_gear_command_exists(self):
        """There should be a canned command that builds a 10-tooth gear without prompts."""

        self.assertRegex(
            self.text,
            r"\(defun SGEARMAKE-TEST10\s*\(\)[\s\S]*:teeth 10",
            "Expect SGEARMAKE-TEST10 to hard-code a 10-tooth property list for programmatic smoke tests.",
        )

        self.assertRegex(
            self.text,
            r"\(defun c:SGEARTEST10\s*\(\)\s*\(SGEARMAKE-TEST10\)",
            "Expect c:SGEARTEST10 to delegate to the canned gear generator without any prompts.",
        )

    def test_minimal_user_input_command_draws_line(self):
        """There should be a simple interactive command that only asks for a length and draws a line."""

        self.assertRegex(
            self.text,
            r"\(defun SGEARLINE\s*\(\)[\s\S]*(sg:prompt-real|getreal)[\s\S]*LINE",
            "Expect SGEARLINE to prompt once for a length (via getreal/sg:prompt-real) and issue a LINE command.",
        )

        self.assertRegex(
            self.text,
            r"\(defun c:SGEARLINE\s*\(\)\s*\(SGEARLINE\)",
            "Expect c:SGEARLINE to delegate to the minimal prompt command.",
        )


if __name__ == "__main__":
    unittest.main()
