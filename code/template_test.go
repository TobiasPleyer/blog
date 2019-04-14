package main
// template_test.go

import (
    "fmt"
    "os"
    "io/ioutil"
    "template"
)


var typeMap = map[template.ItemType]string {
    template.ItemTypeNone: "Empty",
    template.ItemTypeText: "Text",
    template.ItemTypeError: "Error",
    template.ItemTypeEOF: "EOF",
    template.ItemTypeIdentifier: "Identifier",
    template.ItemTypeOperator: "Operator",
    template.ItemTypeLeftMeta: "LeftMeta",
    template.ItemTypeRightMeta: "RightMeta",
}

func main() {
    filename := os.Args[1]
    outfile := os.Args[2]
    var bytes []byte
    var err error

    bytes, err = ioutil.ReadFile(filename)
    if err != nil {
        fmt.Println(err.Error())
        return
    }
    content := string(bytes)

    l := template.Lex("test", content)
    fmt.Println(fmt.Sprintf("Starting to lex\n%v", filename))
    for {
        item := l.NextItem()
        if item.Typ == template.ItemTypeError {
            fmt.Println(item.Val)
        }
        if item.Typ == template.ItemTypeEOF {
            break
        }
        fmt.Printf("(%v,%v)\n", typeMap[item.Typ], item.Val)
    }
    fmt.Println("Finished lexing...")
    fmt.Println(fmt.Sprintf("Rendering template now to\n%v", outfile))

    c := template.Context{
        "print": "true",
        "who": "World",
        "name": "Lexer",
    }
    template.Render(c, filename, outfile)
}
