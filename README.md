# lambda

[![Build Status](https://travis-ci.org/cmc-haskell-2015/lambda.svg?branch=master)](https://travis-ci.org/cmc-haskell-2015/lambda)

Интерпретатор лямбда-исчисления.

## Установка и запуск

Для установки клонируйте репозиторий и запустите `cabal install`:

```
$ git clone https://github.com/cmc-haskell-2015/lambda.git
$ cd lambda
$ cabal install
```

После установки запуск осуществляется командой `lambda`:

```
$ lambda
> (\x y. x y) (\x. x) z
(((\x.(\y.(x y))) (\x.x)) z)
((\y.((\x.x) y)) z)
((\x.x) z)
z
z
```

Для сборки и запуска текущей версии непосредственно из репозитория используйте `cabal run`:

```
$ cd lambda
$ cabal run
```

## Документация

Автоматическая документация кода сгенерирована при помощи [Haddock](https://www.haskell.org/haddock/).

Онлайн документация доступна здесь: http://cmc-haskell-2015.github.io/lambda/docs/

Локально документацию можно собрать, запустив простую команду:

```
$ cabal haddock
```

