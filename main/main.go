package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/tsukanov-as/r2d2/bsl/parser"
)

func parse(path string) {

	defer func() {
		if r := recover(); r != nil {
			fmt.Println(r)
		}
	}()

	var p parser.Parser

	p.Init(path)
	p.Parse()

}

func walker(path string, fileInfo os.FileInfo, err error) error {

	if err != nil {
		return err
	}

	if fileInfo.IsDir() {
		return nil
	}

	if filepath.Ext(path) == ".bsl" {

		go parse(path)

	} else if filepath.Base(path) == "Form.xml" {

		// xmlFile, err := os.Open(path)

		// if err != nil {
		// 	fmt.Println(err)
		// }

		// defer xmlFile.Close()

		// byteValue, _ := ioutil.ReadAll(xmlFile)

		// var mdo conf.ManagedForm
		// xml.Unmarshal(byteValue, &mdo)

	} else if filepath.Ext(path) == ".xml" && filepath.Base(path) != "Template.xml" {

		// xmlFile, err := os.Open(path)

		// if err != nil {
		// 	fmt.Println(err)
		// }

		// defer xmlFile.Close()

		// byteValue, _ := ioutil.ReadAll(xmlFile)

		// var mdo conf.MetaDataObject

		// xml.Unmarshal(byteValue, &mdo)

	}

	return nil
}

func main() {

	err := filepath.Walk("C:/temp/UH", walker)

	if err != nil {
		fmt.Printf("%v\n", err)
	}

}
