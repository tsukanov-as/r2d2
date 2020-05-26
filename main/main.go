package main

import (
	"encoding/xml"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/tsukanov-as/r2d2/bsl/parser"
	"github.com/tsukanov-as/r2d2/conf"
)

func main() {

	var ff = func(pathX string, infoX os.FileInfo, errX error) error {

		if errX != nil {
			fmt.Printf("error 「%v」 at a path 「%q」\n", errX, pathX)
			return errX
		}

		if infoX.IsDir() && "C:\\temp\\UNF\\"+filepath.Base(pathX) == pathX {
			// skip
		} else {

			if filepath.Ext(pathX) == ".bsl" {

				var p parser.Parser

				p.Init(pathX)
				_ = p.Parse()

			} else if filepath.Base(pathX) == "Form.xml" {

				xmlFile, err := os.Open(pathX)

				if err != nil {
					fmt.Println(err)
				}

				defer xmlFile.Close()

				byteValue, _ := ioutil.ReadAll(xmlFile)

				var mdo conf.ManagedForm
				xml.Unmarshal(byteValue, &mdo)

			} else if filepath.Ext(pathX) == ".xml" && filepath.Base(pathX) != "Template.xml" {

				xmlFile, err := os.Open(pathX)

				if err != nil {
					fmt.Println(err)
				}

				defer xmlFile.Close()

				byteValue, _ := ioutil.ReadAll(xmlFile)

				var mdo conf.MetaDataObject

				xml.Unmarshal(byteValue, &mdo)

				_ = 1

			}

		}

		return nil
	}

	err := filepath.Walk("C:/temp/UNF", ff)

	if err != nil {
		fmt.Printf("error walking the path %q: %v\n", "C:/temp/UNF", err)
	}

}
