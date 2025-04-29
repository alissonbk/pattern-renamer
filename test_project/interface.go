package main

type IO interface {
	Open() error
	Read() ([]byte, error)
	Close() error
}
