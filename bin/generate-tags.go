package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

func main() {
	exePath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to get executable path: %v\n", err)
		os.Exit(1)
	}
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(exePath), ".."))
	targetDir := filepath.Join(repoRoot, "packages")

	if hasktagsBin, err := exec.LookPath("hasktags"); err == nil {
		cmd := exec.Command(hasktagsBin, "-c", "-x", "-f", "tags", targetDir)
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "hasktags failed: %v\n", err)
			os.Exit(1)
		}
		fmt.Println("Tags generated successfully in ./tags")
		return
	}

	if ctagsBin, err := exec.LookPath("ctags"); err == nil {
		cmd := exec.Command(ctagsBin, "-R", "-f", "tags", targetDir)
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "ctags failed: %v\n", err)
			os.Exit(1)
		}
		fmt.Println("Tags generated successfully in ./tags")
		return
	}

	fmt.Println("[WARN] Neither hasktags nor ctags found on PATH.")
}
