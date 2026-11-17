package main

import (
	"fmt"
	"os"
	"os/exec"
)

func runCmd(name string, args ...string) {
	cmd := exec.Command(name, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "command failed: %s %v: %v\n", name, args, err)
		os.Exit(1)
	}
}

func main() {
	fmt.Println("=== [TP-SMIRK-003C Verification Gate] ===")
	fmt.Println("  [1/2] Running physical verification suite (bin/smirk-verify)...")
	runCmd("bin/smirk-verify")
	fmt.Println("  [2/2] Running full test suite...")
	runCmd("cabal", "test", "smirk-effectful", "--test-show-details=streaming")
	fmt.Println("=== [TP-SMIRK-003C Verification Gate PASSED] ===")
}
