import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class SetupTests(unittest.TestCase):
    def check_with_versions(self, java_version, javac_version):
        with tempfile.TemporaryDirectory() as directory:
            folder = Path(directory)
            scripts = {
                'java': f"#!/bin/sh\necho '    java.specification.version = {java_version}'\n",
                'javac': f"#!/bin/sh\necho 'javac {javac_version}'\n",
                'scala': '#!/bin/sh\nexit 0\n',
            }
            for name, content in scripts.items():
                path = folder/name
                path.write_text(content)
                path.chmod(0o755)
            environment = dict(os.environ, PATH=str(folder)+os.pathsep+os.environ['PATH'])
            return subprocess.run(['sh', str(ROOT/'scripts/check.sh'), '--quiet'],
                                  env=environment, capture_output=True, text=True)

    def test_rejects_an_old_compiler_even_with_a_new_runtime(self):
        result = self.check_with_versions('22', '17.0.1')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('javac 21 or newer', result.stderr)

    def test_supported_jdks_pass(self):
        self.assertEqual(self.check_with_versions('21', '22.0.1').returncode, 0)
        self.assertNotEqual(self.check_with_versions('17', '22.0.1').returncode, 0)
