# K--

## 문법

제공되는 파서가 정의하고 있는 문법은 조교가 제공한 문서에 있는 expression과 statement가 합쳐있는 언어입니다.

## 우선 순위

사용되는 기호들의 우선 순위는 아래와 같습니다.
위에 있는 기호가 우선 순위가 가장 높고, 아래로 갈수록 우선 순위가 낮아집니다.

* `not` (오른쪽)
* `*`, `/` (왼쪽)
* `+`, `-` (왼쪽)
* `=`, `<` (왼쪽)
* `write` (오른쪽)
* `:=` (오른쪽)
* `else`
* `then`
* `do`
* `;` (왼쪽)
* `in`

우선 순위를 고려해야 되는 몇가지 일반적인 경우를 보여드리면 다음과 같습니다.

```
x := e1 ; e2        =>    (x := e1) ; e2   ( := 이 ; 보다 우선 순위가 높기때문 )
while e do e1;e2    =>    (while e do e1);e2
if e1 then e2 else e3;e4    =>    (if e1 then e2 else e3); e4
let x := e1 in e2 ; e3      =>    let x :=e in (e2;e3)
```

즉 `:=`, `while`, `for`, `if`의 바디로 sequence를 쓰고 싶으면 sequence를 다음과 같이 괄호로 묶어줘야 합니다.

```
while e do (e1;e2)
```

마찬가지로 `let in`의 바디에서는 scope를 제한시키려면 괄호를 쳐줘야 합니다.

```
(let x := e1 in e2); e3
```

방향성은 우선순위가 같은 경우에 적용이 됩니다.
예를 들어 방향성이 오른쪽인 `:=`은

```
x := y := 1    =>   x := (y := 1)
```

이 됩니다.

우선순위를 잘 모르겠을 때는 괄호를 쳐주는 것이 한 방편이 될 수 있습니다.

## 개발 환경 설정

`k__` 디렉토리에서 다음과 같이 opam 환경을 초기화하고 불러올 수 있습니다.
```sh
opam install . --deps-only
eval $(opam env)
```

한 번 opam 환경을 초기화한 후에는 다음과 같이 환경을 불러올 수 있습니다.
```sh
eval $(opam env)
```

opam 환경을 불러온 후에는 아래의 설명에 따라 컴파일 및 실행을 하면 됩니다.

## 컴파일 및 실행 방법

제공되는 k.ml 파일에는 숙제 구현 부분은 비워져 있습니다.
이 파일을 수정해서 interpreter를 완성하고 다음과 같이 컴파일 및 실행을 하면 됩니다.
```sh
dune build --release
./_build/default/bin/main.exe examples/test1.k--
```
혹은 바로
```sh
dune exec --release k__ examples/test1.k--
```

## 숙제 제출 방법

숙제 제출은 `k.ml` 파일만 해주세요.

즉, 조교가 여기서 제공되고 있는 파일 중 `k.ml`만 각자가 제출한 것으로 바꿔서, 컴파일 및 실행이 되도록 제출해 주시기 바랍니다.

## Pretty-printer

입력 프로그램을 간단히 화면에 출력시켜주는 pretty-printer가 제공된 `pp.ml` 파일에 포함되어 있습니다.

사용법은 `dune exec --release k__ -- -pp examples/test1.k--`를 실행하면 `main.ml`에서 `run` 함수를 불러서 intepreter를
돌리는 것이 아니라, `test1.k--`를 파싱해서 입력된 프로그램을 화면에 출력해주고 마치게 됩니다.

이를 통해 파싱이 의도한 대로 되고 있는지 확인해 보실 수 있습니다.

## 주석

K-- 프로그램 안에서 `(*  *)`로 주석을 사용할 수 있습니다.
