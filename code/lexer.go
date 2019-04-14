package template
// lexer.go

import (
    "fmt"
    "strings"
    "unicode/utf8"
)

type ItemType int

const (
    EOF rune = 0
    leftMeta string = "{{"
    rightMeta string = "}}"
)

const (
    Digits string = "0123456789"
    Letters string = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
)

const (
    ItemTypeNone ItemType = iota
    ItemTypeText ItemType = iota
    ItemTypeError ItemType = iota
    ItemTypeEOF ItemType = iota
    ItemTypeIdentifier ItemType = iota
    ItemTypeOperator ItemType = iota
    ItemTypeLeftMeta ItemType = iota
    ItemTypeRightMeta ItemType = iota
)

type Item struct {
    Typ ItemType
    Val string
}

type Lexer struct {
    name string
    input string
    start int
    pos int
    width int
    state stateFn
    items chan Item
}

type stateFn func(*Lexer) stateFn

func Lex(name, input string) *Lexer {
    l := &Lexer{
        name: name,
        input: input,
        state: lexText,
        items: make(chan Item, 2),
    }
    return l
}

// Lexer methods

func (l *Lexer) NextItem() Item {
    for {
        select {
        case item := <-l.items:
            return item
        default:
            l.state = l.state(l)
        }
    }
    panic("unreachable")
}

func (l *Lexer) emit(t ItemType) {
    l.items <- Item{t, l.input[l.start:l.pos]}
    l.start = l.pos
}

func (l *Lexer) next() (r rune) {
    if l.pos >= len(l.input) {
        l.width = 0
        return EOF
    }
    r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
    l.pos += l.width
    return r
}

func (l *Lexer) ignore() {
    l.start = l.pos
}

func (l *Lexer) backup() {
    l.pos -= l.width
}

func (l *Lexer) peek() rune {
    r := l.next()
    l.backup()
    return r
}

func (l *Lexer) accept(valid string) bool {
    if strings.IndexRune(valid, l.next()) >= 0 {
        return true
    }
    l.backup()
    return false
}

func (l *Lexer) acceptAll(valid string) {
    for strings.IndexRune(valid, l.next()) >= 0 {}
    l.backup()
}

func (l *Lexer) skip(invalid string) bool {
    if l.accept(invalid) {
        l.ignore()
        return true
    }
    return false
}

func (l *Lexer) skipAll(invalid string) {
    for l.skip(invalid) {}
}

func isDigit(r rune) bool {
    if strings.IndexRune(Digits, r) >= 0 {
        return true
    }
    return false
}

func isLetter(r rune) bool {
    if strings.IndexRune(Letters, r) >= 0 {
        return true
    }
    return false
}

func isAlphaNumeric(r rune) bool {
    return (isLetter(r) || isDigit(r))
}

// State functions

func (l *Lexer) errorf(format string, args ...interface{}) stateFn {
    l.items <- Item{
        ItemTypeError,
        fmt.Sprintf(format, args...),
    }
    return nil
}

func lexText(l *Lexer) stateFn {
    for {
        if strings.HasPrefix(l.input[l.pos:], leftMeta) {
            if l.pos > l.start {
                l.emit(ItemTypeText)
            }
            return lexLeftMeta
        }
        if l.next() == EOF { break }
    }
    if l.pos > l.start {
        l.emit(ItemTypeText)
    }
    l.emit(ItemTypeEOF)
    return nil
}

func lexLeftMeta(l *Lexer) stateFn {
    l.pos += len(leftMeta)
    l.emit(ItemTypeLeftMeta)
    return lexInsideAction
}

func lexRightMeta(l *Lexer) stateFn {
    l.pos += len(rightMeta)
    l.emit(ItemTypeRightMeta)
    return lexText
}

func lexInsideAction(l *Lexer) stateFn {
    for {
        l.skipAll(" \t")
        if strings.HasPrefix(l.input[l.pos:], rightMeta) {
            return lexRightMeta
        }
        if l.accept(".!") {
            l.emit(ItemTypeOperator)
            return lexIdentifier
        }
        if l.accept("?") {
            l.emit(ItemTypeOperator)
        }
    }
}

func lexIdentifier(l *Lexer) stateFn {
    n := l.next()
    if !isAlphaNumeric(n) {
        return l.errorf("Expected identifier")
    }
    if isDigit(n) {
        return l.errorf("Identifiers must start with a letter")
    }
    for isAlphaNumeric(l.next()){}
    l.backup()
    l.emit(ItemTypeIdentifier)
    return lexInsideAction
}
