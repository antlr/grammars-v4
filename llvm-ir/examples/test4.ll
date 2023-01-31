; ModuleID = 'main.cpp'
source_filename = "main.cpp"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc19.33.31630"

%struct._iobuf = type { i8* }
%struct.__crt_locale_pointers = type { %struct.__crt_locale_data*, %struct.__crt_multibyte_data* }
%struct.__crt_locale_data = type opaque
%struct.__crt_multibyte_data = type opaque

$printf = comdat any

$scanf = comdat any

$_vfprintf_l = comdat any

$__local_stdio_printf_options = comdat any

$_vfscanf_l = comdat any

$__local_stdio_scanf_options = comdat any

$"??_C@_01HMGJMAIH@B?$AA@" = comdat any

$"??_C@_01JPJNBJEM@I?$AA@" = comdat any

$"??_C@_01BIAFAFID@F?$AA@" = comdat any

$"??_C@_02DPKJAMEF@?$CFd?$AA@" = comdat any

$"??_C@_03GNGPFOOL@?$CF1d?$AA@" = comdat any

$"?_OptionsStorage@?1??__local_stdio_printf_options@@9@4_KA" = comdat any

$"?_OptionsStorage@?1??__local_stdio_scanf_options@@9@4_KA" = comdat any

@"?n@@3HA" = dso_local global i32 0, align 4
@"?x@@3HA" = dso_local global i32 0, align 4
@"?a@@3PAHA" = dso_local global [3100 x i32] zeroinitializer, align 16
@"?c@@3DA" = dso_local global i8 0, align 1
@"??_C@_01HMGJMAIH@B?$AA@" = linkonce_odr dso_local unnamed_addr constant [2 x i8] c"B\00", comdat, align 1
@"??_C@_01JPJNBJEM@I?$AA@" = linkonce_odr dso_local unnamed_addr constant [2 x i8] c"I\00", comdat, align 1
@"??_C@_01BIAFAFID@F?$AA@" = linkonce_odr dso_local unnamed_addr constant [2 x i8] c"F\00", comdat, align 1
@"??_C@_02DPKJAMEF@?$CFd?$AA@" = linkonce_odr dso_local unnamed_addr constant [3 x i8] c"%d\00", comdat, align 1
@"??_C@_03GNGPFOOL@?$CF1d?$AA@" = linkonce_odr dso_local unnamed_addr constant [4 x i8] c"%1d\00", comdat, align 1
@"?_OptionsStorage@?1??__local_stdio_printf_options@@9@4_KA" = linkonce_odr dso_local global i64 0, comdat, align 8
@"?_OptionsStorage@?1??__local_stdio_scanf_options@@9@4_KA" = linkonce_odr dso_local global i64 0, comdat, align 8

; Function Attrs: mustprogress noinline optnone uwtable
define dso_local void @"?print@@YAXH@Z"(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = load i32, i32* @"?x@@3HA", align 4
  %5 = mul nsw i32 2, %4
  %6 = icmp sge i32 %3, %5
  br i1 %6, label %7, label %8

7:                                                ; preds = %1
  br label %24

8:                                                ; preds = %1
  %9 = load i32, i32* %2, align 4
  %10 = mul nsw i32 %9, 2
  call void @"?print@@YAXH@Z"(i32 noundef %10)
  %11 = load i32, i32* %2, align 4
  %12 = mul nsw i32 %11, 2
  %13 = add nsw i32 %12, 1
  call void @"?print@@YAXH@Z"(i32 noundef %13)
  %14 = load i32, i32* %2, align 4
  %15 = sext i32 %14 to i64
  %16 = getelementptr inbounds [3100 x i32], [3100 x i32]* @"?a@@3PAHA", i64 0, i64 %15
  %17 = load i32, i32* %16, align 4
  switch i32 %17, label %24 [
    i32 0, label %18
    i32 1, label %20
    i32 2, label %22
  ]

18:                                               ; preds = %8
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @"??_C@_01HMGJMAIH@B?$AA@", i64 0, i64 0))
  br label %24

20:                                               ; preds = %8
  %21 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @"??_C@_01JPJNBJEM@I?$AA@", i64 0, i64 0))
  br label %24

22:                                               ; preds = %8
  %23 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @"??_C@_01BIAFAFID@F?$AA@", i64 0, i64 0))
  br label %24

24:                                               ; preds = %7, %8, %22, %20, %18
  ret void
}

; Function Attrs: mustprogress noinline optnone uwtable
define linkonce_odr dso_local i32 @printf(i8* noundef %0, ...) #0 comdat {
  %2 = alloca i8*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %5 = bitcast i8** %4 to i8*
  call void @llvm.va_start(i8* %5)
  %6 = load i8*, i8** %4, align 8
  %7 = load i8*, i8** %2, align 8
  %8 = call %struct._iobuf* @__acrt_iob_func(i32 noundef 1)
  %9 = call i32 @_vfprintf_l(%struct._iobuf* noundef %8, i8* noundef %7, %struct.__crt_locale_pointers* noundef null, i8* noundef %6)
  store i32 %9, i32* %3, align 4
  %10 = bitcast i8** %4 to i8*
  call void @llvm.va_end(i8* %10)
  %11 = load i32, i32* %3, align 4
  ret i32 %11
}

; Function Attrs: mustprogress noinline norecurse optnone uwtable
define dso_local noundef i32 @main() #1 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %4 = call i32 (i8*, ...) @scanf(i8* noundef getelementptr inbounds ([3 x i8], [3 x i8]* @"??_C@_02DPKJAMEF@?$CFd?$AA@", i64 0, i64 0), i32* noundef @"?n@@3HA")
  %5 = load i32, i32* @"?n@@3HA", align 4
  %6 = shl i32 1, %5
  store i32 %6, i32* @"?x@@3HA", align 4
  %7 = load i32, i32* @"?x@@3HA", align 4
  store i32 %7, i32* %2, align 4
  br label %8

8:                                                ; preds = %18, %0
  %9 = load i32, i32* %2, align 4
  %10 = load i32, i32* @"?x@@3HA", align 4
  %11 = mul nsw i32 2, %10
  %12 = icmp slt i32 %9, %11
  br i1 %12, label %13, label %21

13:                                               ; preds = %8
  %14 = load i32, i32* %2, align 4
  %15 = sext i32 %14 to i64
  %16 = getelementptr inbounds i32, i32* getelementptr inbounds ([3100 x i32], [3100 x i32]* @"?a@@3PAHA", i64 0, i64 0), i64 %15
  %17 = call i32 (i8*, ...) @scanf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @"??_C@_03GNGPFOOL@?$CF1d?$AA@", i64 0, i64 0), i32* noundef %16)
  br label %18

18:                                               ; preds = %13
  %19 = load i32, i32* %2, align 4
  %20 = add nsw i32 %19, 1
  store i32 %20, i32* %2, align 4
  br label %8, !llvm.loop !9

21:                                               ; preds = %8
  %22 = load i32, i32* @"?x@@3HA", align 4
  %23 = mul nsw i32 2, %22
  %24 = sub nsw i32 %23, 1
  store i32 %24, i32* %3, align 4
  br label %25

25:                                               ; preds = %54, %21
  %26 = load i32, i32* %3, align 4
  %27 = icmp sge i32 %26, 2
  br i1 %27, label %28, label %57

28:                                               ; preds = %25
  %29 = load i32, i32* %3, align 4
  %30 = sext i32 %29 to i64
  %31 = getelementptr inbounds [3100 x i32], [3100 x i32]* @"?a@@3PAHA", i64 0, i64 %30
  %32 = load i32, i32* %31, align 4
  %33 = load i32, i32* %3, align 4
  %34 = sub nsw i32 %33, 1
  %35 = sext i32 %34 to i64
  %36 = getelementptr inbounds [3100 x i32], [3100 x i32]* @"?a@@3PAHA", i64 0, i64 %35
  %37 = load i32, i32* %36, align 4
  %38 = icmp eq i32 %32, %37
  br i1 %38, label %39, label %48

39:                                               ; preds = %28
  %40 = load i32, i32* %3, align 4
  %41 = sext i32 %40 to i64
  %42 = getelementptr inbounds [3100 x i32], [3100 x i32]* @"?a@@3PAHA", i64 0, i64 %41
  %43 = load i32, i32* %42, align 4
  %44 = load i32, i32* %3, align 4
  %45 = sdiv i32 %44, 2
  %46 = sext i32 %45 to i64
  %47 = getelementptr inbounds [3100 x i32], [3100 x i32]* @"?a@@3PAHA", i64 0, i64 %46
  store i32 %43, i32* %47, align 4
  br label %53

48:                                               ; preds = %28
  %49 = load i32, i32* %3, align 4
  %50 = sdiv i32 %49, 2
  %51 = sext i32 %50 to i64
  %52 = getelementptr inbounds [3100 x i32], [3100 x i32]* @"?a@@3PAHA", i64 0, i64 %51
  store i32 2, i32* %52, align 4
  br label %53

53:                                               ; preds = %48, %39
  br label %54

54:                                               ; preds = %53
  %55 = load i32, i32* %3, align 4
  %56 = sub nsw i32 %55, 2
  store i32 %56, i32* %3, align 4
  br label %25, !llvm.loop !11

57:                                               ; preds = %25
  call void @"?print@@YAXH@Z"(i32 noundef 1)
  ret i32 0
}

; Function Attrs: mustprogress noinline optnone uwtable
define linkonce_odr dso_local i32 @scanf(i8* noundef %0, ...) #0 comdat {
  %2 = alloca i8*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %5 = bitcast i8** %4 to i8*
  call void @llvm.va_start(i8* %5)
  %6 = load i8*, i8** %4, align 8
  %7 = load i8*, i8** %2, align 8
  %8 = call %struct._iobuf* @__acrt_iob_func(i32 noundef 0)
  %9 = call i32 @_vfscanf_l(%struct._iobuf* noundef %8, i8* noundef %7, %struct.__crt_locale_pointers* noundef null, i8* noundef %6)
  store i32 %9, i32* %3, align 4
  %10 = bitcast i8** %4 to i8*
  call void @llvm.va_end(i8* %10)
  %11 = load i32, i32* %3, align 4
  ret i32 %11
}

; Function Attrs: nofree nosync nounwind willreturn
declare void @llvm.va_start(i8*) #2

; Function Attrs: mustprogress noinline optnone uwtable
define linkonce_odr dso_local i32 @_vfprintf_l(%struct._iobuf* noundef %0, i8* noundef %1, %struct.__crt_locale_pointers* noundef %2, i8* noundef %3) #0 comdat {
  %5 = alloca i8*, align 8
  %6 = alloca %struct.__crt_locale_pointers*, align 8
  %7 = alloca i8*, align 8
  %8 = alloca %struct._iobuf*, align 8
  store i8* %3, i8** %5, align 8
  store %struct.__crt_locale_pointers* %2, %struct.__crt_locale_pointers** %6, align 8
  store i8* %1, i8** %7, align 8
  store %struct._iobuf* %0, %struct._iobuf** %8, align 8
  %9 = load i8*, i8** %5, align 8
  %10 = load %struct.__crt_locale_pointers*, %struct.__crt_locale_pointers** %6, align 8
  %11 = load i8*, i8** %7, align 8
  %12 = load %struct._iobuf*, %struct._iobuf** %8, align 8
  %13 = call i64* @__local_stdio_printf_options()
  %14 = load i64, i64* %13, align 8
  %15 = call i32 @__stdio_common_vfprintf(i64 noundef %14, %struct._iobuf* noundef %12, i8* noundef %11, %struct.__crt_locale_pointers* noundef %10, i8* noundef %9)
  ret i32 %15
}

declare dso_local %struct._iobuf* @__acrt_iob_func(i32 noundef) #3

; Function Attrs: nofree nosync nounwind willreturn
declare void @llvm.va_end(i8*) #2

declare dso_local i32 @__stdio_common_vfprintf(i64 noundef, %struct._iobuf* noundef, i8* noundef, %struct.__crt_locale_pointers* noundef, i8* noundef) #3

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define linkonce_odr dso_local i64* @__local_stdio_printf_options() #4 comdat {
  ret i64* @"?_OptionsStorage@?1??__local_stdio_printf_options@@9@4_KA"
}

; Function Attrs: mustprogress noinline optnone uwtable
define linkonce_odr dso_local i32 @_vfscanf_l(%struct._iobuf* noundef %0, i8* noundef %1, %struct.__crt_locale_pointers* noundef %2, i8* noundef %3) #0 comdat {
  %5 = alloca i8*, align 8
  %6 = alloca %struct.__crt_locale_pointers*, align 8
  %7 = alloca i8*, align 8
  %8 = alloca %struct._iobuf*, align 8
  store i8* %3, i8** %5, align 8
  store %struct.__crt_locale_pointers* %2, %struct.__crt_locale_pointers** %6, align 8
  store i8* %1, i8** %7, align 8
  store %struct._iobuf* %0, %struct._iobuf** %8, align 8
  %9 = load i8*, i8** %5, align 8
  %10 = load %struct.__crt_locale_pointers*, %struct.__crt_locale_pointers** %6, align 8
  %11 = load i8*, i8** %7, align 8
  %12 = load %struct._iobuf*, %struct._iobuf** %8, align 8
  %13 = call i64* @__local_stdio_scanf_options()
  %14 = load i64, i64* %13, align 8
  %15 = call i32 @__stdio_common_vfscanf(i64 noundef %14, %struct._iobuf* noundef %12, i8* noundef %11, %struct.__crt_locale_pointers* noundef %10, i8* noundef %9)
  ret i32 %15
}

declare dso_local i32 @__stdio_common_vfscanf(i64 noundef, %struct._iobuf* noundef, i8* noundef, %struct.__crt_locale_pointers* noundef, i8* noundef) #3

; Function Attrs: mustprogress noinline nounwind optnone uwtable
define linkonce_odr dso_local i64* @__local_stdio_scanf_options() #4 comdat {
  ret i64* @"?_OptionsStorage@?1??__local_stdio_scanf_options@@9@4_KA"
}

attributes #0 = { mustprogress noinline optnone uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress noinline norecurse optnone uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nofree nosync nounwind willreturn }
attributes #3 = { "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { mustprogress noinline nounwind optnone uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.linker.options = !{!0, !1, !2, !3, !4}
!llvm.module.flags = !{!5, !6, !7}
!llvm.ident = !{!8}

!0 = !{!"/FAILIFMISMATCH:\22_CRT_STDIO_ISO_WIDE_SPECIFIERS=0\22"}
!1 = !{!"/FAILIFMISMATCH:\22_MSC_VER=1900\22"}
!2 = !{!"/FAILIFMISMATCH:\22_ITERATOR_DEBUG_LEVEL=0\22"}
!3 = !{!"/FAILIFMISMATCH:\22RuntimeLibrary=MT_StaticRelease\22"}
!4 = !{!"/DEFAULTLIB:libcpmt.lib"}
!5 = !{i32 1, !"wchar_size", i32 2}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 1}
!8 = !{!"clang version 14.0.5"}
!9 = distinct !{!9, !10}
!10 = !{!"llvm.loop.mustprogress"}
!11 = distinct !{!11, !10}
