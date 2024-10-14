import os
import sys
import subprocess

def run_command(command, shell=False):
    result = subprocess.run(command, shell=shell)
    if result.returncode != 0:
        print(f"Command {' '.join(command)} failed.")
        sys.exit(1)

def install_linux():
    run_command(["bash", "-c", "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"])
    run_command(["opam", "init", "-y"])
    run_command(["opam", "install", "-y", "dune", "menhir", "ppx_deriving", "odoc", "llvm14.0.6"], shell=True)

    if os.path.exists("/etc/debian_version"):
        package_manager = "apt"
    elif os.path.exists("/etc/fedora-release"):
        package_manager = "dnf"
    elif os.path.exists("/etc/arch-release"):
        package_manager = "pacman"
    elif os.path.exists("/etc/centos-release") or os.path.exists("/etc/redhat-release"):
        package_manager = "yum"
    else:
        print("Unsupported Linux distribution.")
        sys.exit(1)

    if package_manager == "apt":
        run_command(["sudo", "apt", "install", "-y", "wget", "gnupg"])
        run_command(["wget", "https://apt.llvm.org/llvm.sh"])
        run_command(["chmod", "+x", "llvm.sh"])
        run_command(["sudo", "./llvm.sh", "14.0.6"])
        run_command(["sudo", "apt", "install", "-y", "llvm-14", "llvm-14-dev"])
    
    elif package_manager == "dnf":
        run_command(["sudo", "dnf", "install", "-y", "dnf-plugins-core"])
        run_command(["sudo", "dnf", "config-manager", "--set-enabled", "updates-testing"])
        run_command(["sudo", "dnf", "install", "-y", "llvm14.0.6", "llvm14.0.6-devel"])
    
    elif package_manager == "yum":
        run_command(["sudo", "yum", "install", "-y", "epel-release"])
        run_command(["sudo", "yum", "install", "-y", "llvm14.0.6", "llvm14.0.6-devel"])
    
    elif package_manager == "pacman":
        run_command(["sudo", "pacman", "-Sy", "--noconfirm", "llvm14"])

    print("LLVM 14.0.6 and OCaml installation completed on Linux.")

def install_windows():
    run_command(["winget", "install", "Git.Git"], shell=True)
    run_command(["winget", "install", "OCaml.opam"], shell=True)
    run_command(["opam", "init", "-y"], shell=True)
    run_command(["powershell", "(& opam env) -split '\\r?\\n' | ForEach-Object { Invoke-Expression $_ }"], shell=True)
    run_command(["opam", "install", "-y", "dune", "menhir", "ppx_deriving", "odoc", "llvm14.0.6"], shell=True)

    if subprocess.run(["choco", "--version"], shell=True).returncode != 0:
        print("Chocolatey is required but not installed. Please install Chocolatey from https://chocolatey.org/install")
        sys.exit(1)

    run_command(["choco", "install", "llvm", "--version=14.0.6", "-y"], shell=True)

    print("LLVM 14.0.6 and OCaml installation completed on Windows.")

def main():
    if sys.platform.startswith('linux'):
        install_linux()
    elif sys.platform.startswith('win'):
        install_windows()
    else:
        print(f"Unsupported platform: {sys.platform}")
        sys.exit(1)

if __name__ == "__main__":
    main()
