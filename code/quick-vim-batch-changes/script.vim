function! HelperFunc()
    let lines = []
    call add(lines, "// Automatically generated variables")
    for l in getline("'a", "'b")
        let [all, day, nr, a, b, c, d, e, f, h] = matchlist(l, '^\s\+\(\w\+\) = \(\d\+\),\?$')
        let lower_weekday = tolower(day)
        let source_line = printf("    enum WeekDay %s = %s; // %d", lower_weekday, day, nr)
        call add(lines, source_line)
    endfor
    call add(lines, "// Automatically generated print statements")
    for l in getline("'a", "'b")
        let [all, day, nr, a, b, c, d, e, f, h] = matchlist(l, '^    \(\w\+\) = \(\d\+\),\?$')
        let lower_weekday = tolower(day)
        let source_line = printf("    printf(\"%s is the %%i%%s day of the week\\n\", (int)%s, get_quantifier(%s));", day, lower_weekday, lower_weekday)
        call add(lines, source_line)
    endfor
    call add(lines, "// Autogen End")
    let @a = join(lines, "\n")
    'p | put a
endfunction
