# Interperter of a functional language

## 개요
### 목적
지연평가, 타입추론 등의 기능이 포함된 함수형 언어의 인터프리터

### 일정
- 2019.04 - 2019.06
- 개인 프로젝트

### 사용 기술
O/S : Ubuntu <br>
Language : OCaml<br>

## Build

require ocaml and dune

ocaml 설치방법 참조 : https://ocaml.org/install <br>

dune을 통해 빌드
```bash
dune build
```

## Usage

```bash
dune exec interpreter [file-names]
```
file-names 를 제공하지 않으면 대화형 인터프리터를 실행<br>
file-names 를 제공하면 해당 파일(들)의 실행 결과를 출력

## Examples
type inference and let polymorphism<br>
![type inference and let polymorphism](https://github.com/kimjaebyeog/interpreter_practice/assets/138166161/d60193d4-c6d9-4c26-b813-fc3f8451df8b)

lazy evaluation<br>
![lazy evaluation](https://github.com/kimjaebyeog/interpreter_practice/assets/138166161/d91c84c7-ea74-48d7-b359-d33b54d6d204)

## Syntax
```
(* {x}* represents 0 or more repeats of x *)

<command> ::= <expression> ;;
    | let <variation names> = <expression> ;;
    | let rec <variation names> = <expression> {and <variation names> = <expression>}* ;;

<expression> ::= true | false | <integer> | <variation> | []
    | <expression> + <expression> | <expression> - <expression>
    | <expression> * <expression> | <expression> / <expression>
    | <expression> = <expression> | <expression> < <expression> 
    | (<expression>)
    | <expression> , <expression>
    | <expression> :: <expression>
    | [<expression> {; <expression>}*]
    | fun <variation names> -> <expression>
    | <expression> <expression>
    | if <expression> then <expression> else <expression>
    | match <expression> with <parttern> -> <expression> { | <parttern> -> <expression>}*
    | let <variation names> = <expression> in <expression>
    | let rec <variation names> = <expression> {and <variation names> = <expression>}* in <expression>


<pattern> ::=  true | false | <integer> | <variation> | [] | _
    | (<pattern>)
    | <pattern> , <pattern> 
    | <pattern> :: <pattern>
    | [<pattern> {; <pattern>}*]
    
<type> ::= int | bool | '<integer>
    | <type> list
    | <type> * <type>
    | <type> -> <type>
```

## 해당 프로젝트를 통해 배운점
- 다양한 연산의 순서, 결합 등을 구현하기 위한 구체적인 syntax 의 정의 방법
- thunk 의 추가를 통한 재귀함수, 지연평가 등의 구현
- constraints 와 unification을 통한 type inference의 구현

## 개선점
- 함수의 모듈화
- 더 자세한 정보의 에러 메세지
- 기능 추가: bool 연산, type annotation,3항 이상의 튜플, ... 