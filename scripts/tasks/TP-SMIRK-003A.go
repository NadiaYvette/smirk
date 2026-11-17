package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	if err := os.MkdirAll(filepath.Dir(dst), 0755); err != nil {
		return err
	}

	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, in)
	return err
}

func main() {
	fmt.Println("=== [TP-SMIRK-003A Task Runner (Go)] ===")

	// 1. Copy Budget.hs
	srcBudget := "seeds/TP-SMIRK-003A-Budget.hs"
	dstBudget := "packages/smirk/src/Smirk/Core/Budget.hs"
	if err := copyFile(srcBudget, dstBudget); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcBudget, dstBudget, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstBudget)

	// 2. Copy BudgetSpec.hs
	srcSpec := "seeds/TP-SMIRK-003A-BudgetSpec.hs"
	dstSpec := "packages/smirk/test/BudgetSpec.hs"
	if err := copyFile(srcSpec, dstSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcSpec, dstSpec, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstSpec)

	// 3. Update packages/smirk/smirk.cabal
	cabalPath := "packages/smirk/smirk.cabal"
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)

	if !strings.Contains(cabalStr, "Smirk.Core.Budget") {
		cabalStr = strings.Replace(cabalStr, "        Smirk.Core.AST\n", "        Smirk.Core.AST\n      , Smirk.Core.Budget\n", 1)
	}
	if !strings.Contains(cabalStr, "BudgetSpec") {
		cabalStr = strings.Replace(cabalStr, "        CursorSpec\n", "        BudgetSpec\n      , CursorSpec\n", 1)
	}
	if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Updated %s (exposed Smirk.Core.Budget & BudgetSpec)\n", cabalPath)

	// 4. Update packages/smirk/src/Smirk.hs
	smirkPath := "packages/smirk/src/Smirk.hs"
	smirkBytes, err := os.ReadFile(smirkPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", smirkPath, err)
		os.Exit(1)
	}
	smirkStr := string(smirkBytes)
	if !strings.Contains(smirkStr, "module Smirk.Core.Budget") {
		smirkStr = strings.Replace(smirkStr, "  , module Smirk.Core.Types\n", "  , module Smirk.Core.Types\n  , module Smirk.Core.Budget\n", 1)
		smirkStr = strings.Replace(smirkStr, "import Smirk.Core.Types\n", "import Smirk.Core.Types\nimport Smirk.Core.Budget\n", 1)
		if err := os.WriteFile(smirkPath, []byte(smirkStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", smirkPath, err)
			os.Exit(1)
		}
		fmt.Printf("  [+] Updated %s (re-exported Smirk.Core.Budget)\n", smirkPath)
	}

	// 5. Update packages/smirk/test/Spec.hs
	specPath := "packages/smirk/test/Spec.hs"
	specBytes, err := os.ReadFile(specPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", specPath, err)
		os.Exit(1)
	}
	specStr := string(specBytes)
	if !strings.Contains(specStr, "BudgetSpec") {
		specStr = strings.Replace(specStr, "import qualified CursorSpec\n", "import qualified BudgetSpec\nimport qualified CursorSpec\n", 1)
		specStr = strings.Replace(specStr, "  CursorSpec.spec\n", "  BudgetSpec.spec\n  CursorSpec.spec\n", 1)
		if err := os.WriteFile(specPath, []byte(specStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specPath, err)
			os.Exit(1)
		}
		fmt.Printf("  [+] Updated %s (wired BudgetSpec.spec)\n", specPath)
	}

	fmt.Println("=== [TP-SMIRK-003A Task Runner Complete] ===")
}
