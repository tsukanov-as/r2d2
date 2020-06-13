package main

import (
	"fmt"
	"os"
	"path/filepath"
	"sync"

	"github.com/tsukanov-as/r2d2/bsl/ast"
	"github.com/tsukanov-as/r2d2/bsl/parser"
	"github.com/tsukanov-as/r2d2/plugins"
)

var wg sync.WaitGroup

func parse(path string) {

	defer func() {
		wg.Done()
		if r := recover(); r != nil {
			fmt.Println(r)
		}
	}()

	wg.Add(1)

	p := &parser.Parser{}

	p.Init(path)
	m := p.Parse()

	v := &ast.Visitor{}
	plist := []interface{}{}
	for i := 0; i < 1000; i++ {
		plist = append(plist, plugins.PluginWrongComment(p))
	}
	v.HookUp(plist)

	m.Accept(v)

	// fmt.Printf("%v\n", len(m.Decls))

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

	wg.Wait()

}
