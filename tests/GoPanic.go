package main

import (
	"errors"
	"fmt"
)

type BadInputError struct {
	input string
}

func (e *BadInputError) Error() string {
	return fmt.Sprintf("bad input: %s", e.input)
}

func main() {
	defer func() {
		err := recover().(error)
		var badInputErr *BadInputError
		if errors.As(err, &badInputErr) {
			fmt.Printf("bad input error occurred: %s\n", badInputErr)
		}
	}()

	panic(fmt.Errorf("validateInput: %w", &BadInputError{input: "xyz"}))
}
