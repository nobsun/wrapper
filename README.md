# wrapper

REPLとのセッションを記録するためのラッパー

## fizzbuzzw

ダミーREPL、fizzbuzz（このパッケージに同梱）のラッパー

## ghciw

ghci のラッパー

### ghciw の機能

- `:load` 命令を発行する前に`:edit`コマンドを発行して、ソースコードを行番号付きで表示する
- `:reload` 命令は`:edit`命令に置き換える
- `ghci.log`にセッションを記録
    - `ghci` への入力行 + 入力時のタイムスタンプ
    - `ghci` からの応答 + 応答時のタイムスタンプ

以下の制限事項がある

- `.ghci` ファイルが以下のように設定されていること前提とする
    - ghci のプロンプトは `>>> ` 固定
    - ghci の `:edit` コマンドは、ソースを行番号付き表示するだけの `catn`（このパッケージに同梱）を呼び出すだけ

```
:set prompt ">>> "
:set editor catn
```

- `ghci` の複数行入力には未対応
- 一部の `ghci`コマンドは機能しない
- `ghci` のデバッガ機能については未対応（そもそも未確認）