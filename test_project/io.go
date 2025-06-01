package main

import (
	"fmt"
	"os"
)

var _ IO = (*TextFile)(nil)

type TextFile struct {
	TextFileName  string `json:"textFileName" db:"textFileName"`
	fileReference *os.File
	content       []byte
}

func NewTextFile(filename string) *TextFile {
	return &TextFile{TextFileName: filename}
}

func (t *TextFile) Open() error {
	f, err := os.Open(t.TextFileName)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to open the text file, reason: %s", err.Error())
		return err
	}
	t.fileReference = f
	return nil
}

func (t *TextFile) Read() ([]byte, error) {
	if t.fileReference == nil {
		return nil, fmt.Errorf("text file was not openned yet!")
	}
	if t.content != nil && len(t.content) > 0 {
		return t.content, nil
	}
	return os.ReadFile(t.TextFileName)
}

func (t *TextFile) Close() error {
	return t.fileReference.Close()
}
