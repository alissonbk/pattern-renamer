package main

import (
	"context"
	"database/sql"
)

type DB struct {
	conn *sql.Conn
	ctx  context.Context
}

func NewDB() *DB {
	return &DB{conn: nil, ctx: context.TODO()}
}

/*
* Some doc with the word text file
 */
func (db *DB) SaveTextFile(textFile *TextFile) error {
	content, err := textFile.Read()
	if err != nil {
		return err
	}
	_, err = db.conn.QueryContext(
		db.ctx,
		"insert into text_file (text_file_content, text_file_name) VALUES ($1, $2)",
		content, textFile.TextFileName)

	return err
}
