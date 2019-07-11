mkdir -p $out
for i in $paths; do
  /nix/store/n9x3d54nkv819ifbxla3aqj5bxzrvrkz-lndir-1.0.3/bin/lndir -silent $i $out
done
. /nix/store/l1d26c47xx8z46cgp9spy2x868bkl0r9-hook/nix-support/setup-hook

# wrap compiler executables with correct env variables

for prg in ghc ghci ghc-8.6.3 ghci-8.6.3; do
  if [[ -x \"/nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/$prg\" ]]; then
    rm -f $out/bin/$prg
    makeWrapper /nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/$prg $out/bin/$prg                           \\
      --add-flags '\"-B$NIX_GHC_LIBDIR\"'                   \\
      --set \"NIX_GHC\"        \"$out/bin/ghc\"     \\
      --set \"NIX_GHCPKG\"     \"$out/bin/ghc-pkg\" \\
      --set \"NIX_GHC_DOCDIR\" \"$out/share/doc/ghc/html\"                  \\
      --set \"NIX_GHC_LIBDIR\" \"$out/lib/ghc-8.6.3\"                  \\
       \\
      
  fi
done

for prg in runghc runhaskell; do
  if [[ -x \"/nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/$prg\" ]]; then
    rm -f $out/bin/$prg
    makeWrapper /nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/$prg $out/bin/$prg                           \\
      --add-flags \"-f $out/bin/ghc\"                           \\
      --set \"NIX_GHC\"        \"$out/bin/ghc\"     \\
      --set \"NIX_GHCPKG\"     \"$out/bin/ghc-pkg\" \\
      --set \"NIX_GHC_DOCDIR\" \"$out/share/doc/ghc/html\"                  \\
      --set \"NIX_GHC_LIBDIR\" \"$out/lib/ghc-8.6.3\"
  fi
done

for prg in ghc-pkg ghc-pkg-8.6.3; do
  if [[ -x \"/nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/$prg\" ]]; then
    rm -f $out/bin/$prg
    makeWrapper /nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/$prg $out/bin/$prg --add-flags \"--global-package-db=$out/lib/ghc-8.6.3/package.conf.d\"
  fi
done

# haddock was referring to the base ghc, https://github.com/NixOS/nixpkgs/issues/36976
if [[ -x \"/nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/haddock\" ]]; then
  rm -f $out/bin/haddock
  makeWrapper /nix/store/7874h075nf8yikvr47642xqrwqwyv99s-ghc-8.6.3/bin/haddock $out/bin/haddock    \\
    --add-flags '\"-B$NIX_GHC_LIBDIR\"'  \\
    --set \"NIX_GHC_LIBDIR\" \"$out/lib/ghc-8.6.3\"
fi

$out/bin/ghc-pkg recache

$out/bin/ghc-pkg check

