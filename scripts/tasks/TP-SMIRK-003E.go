package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func copyFile(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(dst), 0755); err != nil {
		return err
	}
	return os.WriteFile(dst, data, 0644)
}

func main() {
	fmt.Println("=== [TP-SMIRK-003E Task Runner] ===")
	cwd, err := os.Getwd()
	if err != nil {
		panic(err)
	}

	seedsDir := filepath.Join(cwd, "seeds")
	if _, err := os.Stat(seedsDir); os.IsNotExist(err) {
		seedsDir = "/home/nyc/src/plans/seeds"
	}

	telemetrySeed := filepath.Join(seedsDir, "TP-SMIRK-003E-Telemetry.hs")
	specSeed := filepath.Join(seedsDir, "TP-SMIRK-003E-Spec.hs")

	targetTelemetry := filepath.Join(cwd, "packages", "smirk-penelope", "src", "Smirk", "Effect", "Telemetry.hs")
	targetSpec := filepath.Join(cwd, "packages", "smirk-penelope", "test", "Spec.hs")

	fmt.Printf("Mounting seed: %s -> %s\n", telemetrySeed, targetTelemetry)
	if err := copyFile(telemetrySeed, targetTelemetry); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy telemetry module: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Mounting seed: %s -> %s\n", specSeed, targetSpec)
	if err := copyFile(specSeed, targetSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy spec module: %v\n", err)
		os.Exit(1)
	}

	// Update smirk-penelope.cabal
	cabalPath := filepath.Join(cwd, "packages", "smirk-penelope", "smirk-penelope.cabal")
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)

	if !strings.Contains(cabalStr, "Smirk.Effect.Telemetry") {
		cabalStr = strings.Replace(
			cabalStr,
			"    exposed-modules:\n        Smirk.Effect.Penelope\n    build-depends:\n        base >= 4.16 && < 5,",
			"    exposed-modules:\n        Smirk.Effect.Penelope\n      , Smirk.Effect.Telemetry\n    build-depends:\n        base >= 4.16 && < 5,\n        dunning-t-digest >= 0.1,",
			1,
		)
		cabalStr = strings.Replace(
			cabalStr,
			"test-suite smirk-penelope-test\n    import:             common-options\n    type:               exitcode-stdio-1.0\n    hs-source-dirs:     test\n    main-is:            Spec.hs\n    ghc-options:\n        -fplugin=Polytropos.Plugin\n    build-depends:\n        base >= 4.16 && < 5,",
			"test-suite smirk-penelope-test\n    import:             common-options\n    type:               exitcode-stdio-1.0\n    hs-source-dirs:     test\n    main-is:            Spec.hs\n    ghc-options:\n        -fplugin=Polytropos.Plugin\n    build-depends:\n        base >= 4.16 && < 5,\n        dunning-t-digest >= 0.1,",
			1,
		)
	}
	if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
		os.Exit(1)
	}

	// Ensure cabal.project.local has t-digest
	cplPath := filepath.Join(cwd, "cabal.project.local")
	if cplBytes, err := os.ReadFile(cplPath); err == nil {
		cplStr := string(cplBytes)
		if !strings.Contains(cplStr, "/home/nyc/src/t-digest/haskell") {
			cplStr = strings.Replace(
				cplStr,
				"/home/nyc/src/penelope/packages/penelope-effects",
				"/home/nyc/src/penelope/packages/penelope-effects\n  /home/nyc/src/t-digest/haskell",
				1,
			)
			_ = os.WriteFile(cplPath, []byte(cplStr), 0644)
		}
	}

	fmt.Println("=== [TP-SMIRK-003E Setup Complete] ===")
}
