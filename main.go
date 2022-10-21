package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	if len(os.Args) > 2 {
		fmt.Println("Usage: lox [script]")
		os.Exit(64)
	} else if len(os.Args) == 2 {
		runFile(os.Args[0])
	} else {
		runPrompt()
	}
}

func runFile(path string) {
	bytes, err := ioutil.ReadFile(path)
	if err != nil {
		fmt.Printf("Failed to read file: %s\n", path)
		os.Exit(1)
	}

	run(bytes)
}

func runPrompt() {
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("> ")
		line, err := reader.ReadBytes('\n')
		if err != nil {
			fmt.Printf("Failed to read line\n")
			os.Exit(1)
		}
		if line == nil {
			break
		}
		run(line)
	}
}

func run(bytes []byte) {
	fmt.Println(string(bytes))
}
