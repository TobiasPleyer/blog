package template
// parser.go

import (
    "os"
    "fmt"
    "strings"
    "io/ioutil"
)

type Context map[string]string

type Parser struct {
    l *Lexer
    curr Item
}


func check(e error) {
    if e != nil {
        panic(e)
    }
}

var itemDot Item = Item {
    Typ: ItemTypeOperator,
    Val: ".",
}

var itemRightMeta Item = Item {
    Typ: ItemTypeRightMeta,
    Val: rightMeta,
}

func printFunc (filepath string, ch chan string) {
    f, err := os.Create(filepath)
    check(err)
    defer f.Close()
    defer f.Sync()
    for {
        input, chanOpen := <-ch
        if !chanOpen { break }
        _, err := f.WriteString(input)
        check(err)
    }
}

func (p *Parser) advance() ItemType {
    p.curr = p.l.NextItem()
    return p.curr.Typ
}

func (p *Parser) dropUntil(typ ItemType) {
    for {
        p.curr = p.l.NextItem()
        if p.curr.Typ == typ {
            break
        }
        if p.curr.Typ == ItemTypeEOF {
            panic("Unexpected EOF")
        }
        if p.curr.Typ == ItemTypeError {
            panic("Unexpected Error")
        }
    }
}

func (p *Parser) expectType(typ ItemType) string {
    p.curr = p.l.NextItem()
    if p.curr.Typ != typ {
        panic(fmt.Sprintf("Expected %v, got %v", typ, p.curr.Typ))
    }
    return p.curr.Val
}

func (p *Parser) expectItem(item Item) {
    p.curr = p.l.NextItem()
    if p.curr.Typ != item.Typ || p.curr.Val != item.Val {
        panic(fmt.Sprintf("Expected (%v,%v), got (%v,%v)", item.Typ, item.Val, p.curr.Typ, p.curr.Val))
    }
}

func parseMeta(c Context, p *Parser) string {
    var result string = ""
    var guard bool = true
    var ident string
    var mod string
    var op string
    var typ ItemType
    var secondRun = false
begin:
    p.expectItem(itemDot)
    ident = p.expectType(ItemTypeIdentifier)
    typ = p.advance()
    if typ == ItemTypeRightMeta {
        result = c[ident]
        goto end
    }
    if typ == ItemTypeOperator {
        op = p.curr.Val
        switch op {
        case "?":
            if op == "?" {
                if secondRun {
                    panic("Only one guard allowed per meta expression!")
                }
                secondRun = true
                guard = c[ident] == "true"
                goto begin
            }
        case "!":
            mod = p.expectType(ItemTypeIdentifier)
            if mod == "lower" {
                result = strings.ToLower(c[ident])
            }
            if mod == "upper" {
                result = strings.ToUpper(c[ident])
            }
            if mod != "lower" && mod != "upper" {
                panic(fmt.Sprintf("Unsupported modifier %v", mod))
            }
            p.expectItem(itemRightMeta)
        default:
            panic(fmt.Sprintf("Unsupported operator %v", op))
        }
    }
    if typ != ItemTypeOperator && typ != ItemTypeRightMeta {
        panic(fmt.Sprintf("Unexpected type %v", typ))
    }
end:
    if guard {
        return result
    }
    return ""
}

// The Render function takes a context, a template file and an output file and
// then renders the template into the output file
func Render(context Context, template string, outfile string) {
    bytes, err := ioutil.ReadFile(template)
    check(err)
    content := string(bytes)
    l := Lex("TemplateLexer", content)
    printCh := make(chan string)
    go printFunc(outfile, printCh)
    var typ ItemType
    var p *Parser = &Parser{
        l: l,
        curr: Item{Typ: ItemTypeNone, Val:"",},
    }
    for {
        typ = p.advance()
        if typ == ItemTypeEOF {
            break
        }
        if typ == ItemTypeError {
            panic(fmt.Sprintf("Error: %v", p.curr.Val))
        }
        if typ == ItemTypeText {
            printCh <- p.curr.Val
            continue
        }
        if typ == ItemTypeLeftMeta {
            meta := parseMeta(context, p)
            if meta != "" {
                printCh <- meta
            }
            continue
        }
        panic(fmt.Sprintf("Unexpected token: (%v,%v)", p.curr.Typ, p.curr.Val))
    }
    close(printCh)
}
