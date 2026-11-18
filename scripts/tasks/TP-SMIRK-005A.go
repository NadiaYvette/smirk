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

func findSeed(name string) string {
	candidates := []string{
		filepath.Join("seeds", name),
		filepath.Join(os.Getenv("HOME"), "src/plans/seeds", name),
		filepath.Join("..", "seeds", name),
	}
	for _, c := range candidates {
		if _, err := os.Stat(c); err == nil {
			return c
		}
	}
	return filepath.Join("seeds", name)
}

func main() {
	fmt.Println("=== [TP-SMIRK-005A Task Runner (Go)] ===")

	// 1. Copy Callout.hs
	srcCallout := findSeed("TP-SMIRK-005A-Callout.hs")
	dstCallout := "packages/smirk-penelope/src/Smirk/Effect/Callout.hs"
	if err := copyFile(srcCallout, dstCallout); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcCallout, dstCallout, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstCallout)

	// 2. Copy CalloutSpec.hs
	srcSpec := findSeed("TP-SMIRK-005A-CalloutSpec.hs")
	dstSpec := "packages/smirk-penelope/test/CalloutSpec.hs"
	if err := copyFile(srcSpec, dstSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcSpec, dstSpec, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstSpec)

	// 3. Update packages/smirk-penelope/smirk-penelope.cabal
	cabalPath := "packages/smirk-penelope/smirk-penelope.cabal"
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)
	if !strings.Contains(cabalStr, "Smirk.Effect.Callout") {
		cabalStr = strings.Replace(cabalStr,
			"      , Smirk.Continuation.Cooperative\n",
			"      , Smirk.Continuation.Cooperative\n      , Smirk.Effect.Callout\n", 1)
	}
	if !strings.Contains(cabalStr, "CalloutSpec") {
		cabalStr = strings.Replace(cabalStr,
			"    hs-source-dirs:     test\n    main-is:            Spec.hs\n",
			"    hs-source-dirs:     test\n    main-is:            Spec.hs\n    other-modules:\n        CalloutSpec\n", 1)
	}
	if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	fmt.Println("  [+] Updated smirk-penelope.cabal")

	// 4. Update packages/smirk-penelope/test/Spec.hs
	specPath := "packages/smirk-penelope/test/Spec.hs"
	specBytes, err := os.ReadFile(specPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", specPath, err)
		os.Exit(1)
	}
	specStr := string(specBytes)
	if !strings.Contains(specStr, "import qualified CalloutSpec") {
		specStr = strings.Replace(specStr,
			"import Smirk.Effect.Telemetry\n",
			"import Smirk.Effect.Telemetry\nimport qualified CalloutSpec\n", 1)
	}
	if !strings.Contains(specStr, "CalloutSpec.spec") {
		specStr = strings.Replace(specStr,
			"main :: IO ()\nmain = hspec $ do\n",
			"main :: IO ()\nmain = hspec $ do\n  CalloutSpec.spec\n", 1)
	}
	if err := os.WriteFile(specPath, []byte(specStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specPath, err)
		os.Exit(1)
	}
	fmt.Println("  [+] Updated smirk-penelope/test/Spec.hs")

	fmt.Println("=== [TP-SMIRK-005A Task Runner Completed Successfully] ===")
}
