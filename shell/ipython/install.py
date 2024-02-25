import subprocess
import os
from pathlib import Path, WindowsPath

scriptdir = Path(__file__).parent
windows = False
try:
    import psutil

    pprocName = psutil.Process(os.getppid()).name()
    if pprocName == "winpty-agent.exe":
        # git bash
        windows = True
except ModuleNotFoundError:
    pass


def set_up():
    ipython_dir = Path("~/.ipython").expanduser()
    subprocess.run(["ipython", "profile", "create", "--ipython-dir", str(ipython_dir)])
    default_dir = ipython_dir / "profile_default"
    startup_dir = default_dir / "startup"
    config = scriptdir / "ipython.py"
    base_rc = default_dir / "ipython_config.py"
    default_dir.mkdir(exist_ok=True)
    if windows:
        base_rc = WindowsPath(base_rc)
    # need to set dotfiles_config_dir for other files.
    install_text = f"c.BaseIPythonApplication.extra_config_file = r'{str(config)}'"
    if not base_rc.exists() or install_text not in base_rc.read_text():
        with base_rc.open("a") as f:
            f.write(install_text)


if __name__ == "__main__":
    set_up()
