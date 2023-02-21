target triple = "x86_64-pc-linux-gnu"

@.fmtstr = private unnamed_addr constant [5 x i8] c"%d \0A\00", align 1
@.fizz = private unnamed_addr constant [6 x i8] c"Fizz \00", align 1
@.buzz = private unnamed_addr constant [6 x i8] c"Buzz \00", align 1
@.nl = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@0 = private unnamed_addr constant [13 x i8] c"Hello world!\00", align 1
@"😎" = private unnamed_addr constant [15 x i8] c"Goodbye world!\00", align 1

declare noundef i32 @printf(i8* nocapture noundef readonly, ...) local_unnamed_addr

define i32 @main() {
  br label %"funny:label"

"funny:label":
  br label %"😊"

"😊":
  br label %"\68ello"

hello:
  br label %before

before:
  %pfout = call i32 (i8*, ...) @printf(i8* getelementptr  ([5 x i8], [5 x i8]* @.fmtstr, i64 0, i64 0), i32 69)
  br label %loopbegin

loopbegin:
  %i = phi i32 [ 0, %before ], [ %nexti, %loopend ]
  %fbcount = add i32 0, 0

  %loopcond = icmp ule i32 %i, 50
  br i1 %loopcond, label %loopbody, label %loopout

loopbody:
  %fizzrem = urem i32 %i, 3
  %fizzcond = icmp eq i32 %fizzrem, 0
  br i1 %fizzcond, label %fizz, label %afterfizz

fizz:
  %pfoutF = call i32 (i8*, ...) @printf(i8* getelementptr ([6 x i8], [6 x i8]* @.fizz, i64 0, i64 0))
  br label %afterfizz

afterfizz:
  %didfizz = phi i1 [ 0, %loopbody ], [ 1, %fizz ]
  %buzzrem = urem i32 %i, 5
  %buzzcond = icmp eq i32 %buzzrem, 0
  br i1 %buzzcond, label %buzz, label %afterbuzz

buzz:
  %pfoutB = call i32 (i8*, ...) @printf(i8* getelementptr ([6 x i8], [6 x i8]* @.buzz, i64 0, i64 0))
  br label %afterbuzz

afterbuzz:
  %didbuzz = phi i1 [ 0, %afterfizz ], [ 1, %buzz ]
  %didfizzorbuzz = or i1 %didfizz, %didbuzz

  br i1 %didfizzorbuzz, label %printnl, label %printi

printnl:
  %pfoutNL = call i32 (i8*, ...) @printf(i8* getelementptr ([2 x i8], [2 x i8]* @.nl, i64 0, i64 0))
  br label %loopend

printi:
  %pfoutnum = call i32 (i8*, ...) @printf(i8* getelementptr ([5 x i8], [5 x i8]* @.fmtstr, i64 0, i64 0), i32 %i)
  br label %loopend

loopend:
  %nexti = add i32 %i, 1
  br label %loopbegin

loopout:
  ret i32 20
}