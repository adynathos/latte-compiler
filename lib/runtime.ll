; ModuleID = 'runtime.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@strBuffer = common global [2048 x i8] zeroinitializer, align 16
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str2 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str3 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str4 = private unnamed_addr constant [15 x i8] c"runtime error\0A\00", align 1

; Function Attrs: nounwind uwtable
define i8* @$internal_return_string_from_buffer(i8* %buffer) #0 {
  %1 = alloca i8*, align 8
  %retStr = alloca i8*, align 8
  %len = alloca i32, align 4
  store i8* %buffer, i8** %1, align 8
  %2 = load i8** %1, align 8
  %3 = call i64 @strlen(i8* %2) #5
  %4 = trunc i64 %3 to i32
  store i32 %4, i32* %len, align 4
  %5 = load i32* %len, align 4
  %6 = add nsw i32 %5, 1
  %7 = sext i32 %6 to i64
  %8 = mul i64 1, %7
  %9 = call noalias i8* @malloc(i64 %8) #6
  store i8* %9, i8** %retStr, align 8
  %10 = load i8** %retStr, align 8
  %11 = load i8** %1, align 8
  %12 = call i8* @strcpy(i8* %10, i8* %11) #6
  %13 = load i8** %retStr, align 8
  ret i8* %13
}

; Function Attrs: nounwind readonly
declare i64 @strlen(i8*) #1

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #2

; Function Attrs: nounwind
declare i8* @strcpy(i8*, i8*) #2

; Function Attrs: nounwind uwtable
define i8* @$internal_string_add(i8* %a, i8* %b) #0 {
  %1 = alloca i8*, align 8
  %2 = alloca i8*, align 8
  store i8* %a, i8** %1, align 8
  store i8* %b, i8** %2, align 8
  store i8 0, i8* getelementptr inbounds ([2048 x i8]* @strBuffer, i32 0, i64 0), align 1
  %3 = load i8** %1, align 8
  %4 = call i8* @strcat(i8* getelementptr inbounds ([2048 x i8]* @strBuffer, i32 0, i32 0), i8* %3) #6
  %5 = load i8** %2, align 8
  %6 = call i8* @strcat(i8* getelementptr inbounds ([2048 x i8]* @strBuffer, i32 0, i32 0), i8* %5) #6
  %7 = call i8* @$internal_return_string_from_buffer(i8* getelementptr inbounds ([2048 x i8]* @strBuffer, i32 0, i32 0))
  ret i8* %7
}

; Function Attrs: nounwind
declare i8* @strcat(i8*, i8*) #2

; Function Attrs: nounwind uwtable
define i1 @$internal_string_compare(i8* %a, i8* %b, i32 %op) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i8*, align 8
  %3 = alloca i8*, align 8
  %4 = alloca i32, align 4
  %cmp = alloca i32, align 4
  store i8* %a, i8** %2, align 8
  store i8* %b, i8** %3, align 8
  store i32 %op, i32* %4, align 4
  %5 = load i8** %2, align 8
  %6 = load i8** %3, align 8
  %7 = call i32 @strcmp(i8* %5, i8* %6) #5
  store i32 %7, i32* %cmp, align 4
  %8 = load i32* %4, align 4
  switch i32 %8, label %33 [
    i32 0, label %9
    i32 1, label %13
    i32 2, label %17
    i32 3, label %21
    i32 4, label %25
    i32 5, label %29
  ]

; <label>:9                                       ; preds = %0
  %10 = load i32* %cmp, align 4
  %11 = icmp eq i32 %10, 0
  %12 = zext i1 %11 to i32
  store i32 %12, i32* %1
  br label %34

; <label>:13                                      ; preds = %0
  %14 = load i32* %cmp, align 4
  %15 = icmp ne i32 %14, 0
  %16 = zext i1 %15 to i32
  store i32 %16, i32* %1
  br label %34

; <label>:17                                      ; preds = %0
  %18 = load i32* %cmp, align 4
  %19 = icmp sge i32 %18, 0
  %20 = zext i1 %19 to i32
  store i32 %20, i32* %1
  br label %34

; <label>:21                                      ; preds = %0
  %22 = load i32* %cmp, align 4
  %23 = icmp eq i32 %22, 1
  %24 = zext i1 %23 to i32
  store i32 %24, i32* %1
  br label %34

; <label>:25                                      ; preds = %0
  %26 = load i32* %cmp, align 4
  %27 = icmp sle i32 %26, 0
  %28 = zext i1 %27 to i32
  store i32 %28, i32* %1
  br label %34

; <label>:29                                      ; preds = %0
  %30 = load i32* %cmp, align 4
  %31 = icmp eq i32 %30, -1
  %32 = zext i1 %31 to i32
  store i32 %32, i32* %1
  br label %34

; <label>:33                                      ; preds = %0
  store i32 0, i32* %1
  br label %34

; <label>:34                                      ; preds = %33, %29, %25, %21, %17, %13, %9
  %35 = load i32* %1
  %result = trunc i32 %35 to i1
  ret i1 %result
}

; Function Attrs: nounwind readonly
declare i32 @strcmp(i8*, i8*) #1

; Function Attrs: nounwind uwtable
define void @printInt(i32 %int_val) #0 {
  %1 = alloca i32, align 4
  store i32 %int_val, i32* %1, align 4
  %2 = load i32* %1, align 4
  %3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 %2)
  ret void
}

declare i32 @printf(i8*, ...) #3

; Function Attrs: nounwind uwtable
define void @printString(i8* %str) #0 {
  %1 = alloca i8*, align 8
  store i8* %str, i8** %1, align 8
  %2 = load i8** %1, align 8
  %3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), i8* %2)
  ret void
}

; Function Attrs: nounwind uwtable
define i32 @readInt() #0 {
  %int_val = alloca i32, align 4
  %1 = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([3 x i8]* @.str2, i32 0, i32 0), i32* %int_val)
  %2 = load i32* %int_val, align 4
  ret i32 %2
}

declare i32 @__isoc99_scanf(i8*, ...) #3

; Function Attrs: nounwind uwtable
define i8* @readString() #0 {
  %1 = call i32 (i8*, ...)* @__isoc99_scanf(i8* getelementptr inbounds ([3 x i8]* @.str3, i32 0, i32 0), i8* getelementptr inbounds ([2048 x i8]* @strBuffer, i32 0, i32 0))
  %2 = call i8* @$internal_return_string_from_buffer(i8* getelementptr inbounds ([2048 x i8]* @strBuffer, i32 0, i32 0))
  ret i8* %2
}

; Function Attrs: nounwind uwtable
define void @error() #0 {
  %1 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str4, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable
                                                  ; No predecessors!
  ret void
}

; Function Attrs: noreturn nounwind
declare void @exit(i32) #4

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readonly "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { noreturn nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind readonly }
attributes #6 = { nounwind }
attributes #7 = { noreturn nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Ubuntu clang version 3.4-1ubuntu3 (tags/RELEASE_34/final) (based on LLVM 3.4)"}
