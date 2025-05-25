; ModuleID = 'main.c'
source_filename = "main.c"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc19.33.31630"

%struct.EDGE = type { i32, i32, %struct.EDGE* }
%struct.NODE = type { i32, i32 }
%struct.__crt_locale_pointers = type { %struct.__crt_locale_data*, %struct.__crt_multibyte_data* }
%struct.__crt_locale_data = type opaque
%struct.__crt_multibyte_data = type opaque
%struct._iobuf = type { i8* }

$sprintf = comdat any

$vsprintf = comdat any

$_snprintf = comdat any

$_vsnprintf = comdat any

$scanf = comdat any

$printf = comdat any

$_vsprintf_l = comdat any

$_vsnprintf_l = comdat any

$__local_stdio_printf_options = comdat any

$_vfscanf_l = comdat any

$__local_stdio_scanf_options = comdat any

$_vfprintf_l = comdat any

$"??_C@_06OOABJBHO@?$CFd?$CFd?$CFd?$AA@" = comdat any

$"??_C@_03JDANDILB@?$CFd?5?$AA@" = comdat any

@edge = dso_local global [100010 x %struct.EDGE] zeroinitializer, align 16
@heapSize = dso_local global i32 0, align 4
@heap = dso_local global [100010 x %struct.NODE] zeroinitializer, align 16
@start = dso_local global i32 0, align 4
@m = dso_local global i32 0, align 4
@n = dso_local global i32 0, align 4
@"??_C@_06OOABJBHO@?$CFd?$CFd?$CFd?$AA@" = linkonce_odr dso_local unnamed_addr constant [7 x i8] c"%d%d%d\00", comdat, align 1
@dis = dso_local global [100010 x i32] zeroinitializer, align 16
@sure = dso_local global [100010 x i32] zeroinitializer, align 16
@"??_C@_03JDANDILB@?$CFd?5?$AA@" = linkonce_odr dso_local unnamed_addr constant [4 x i8] c"%d \00", comdat, align 1
@__local_stdio_printf_options._OptionsStorage = internal global i64 0, align 8
@__local_stdio_scanf_options._OptionsStorage = internal global i64 0, align 8

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local i32 @sprintf(i8* noundef %0, i8* noundef %1, ...) #0 comdat {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i8*, align 8
  store i8* %1, i8** %3, align 8
  store i8* %0, i8** %4, align 8
  %7 = bitcast i8** %6 to i8*
  call void @llvm.va_start(i8* %7)
  %8 = load i8*, i8** %6, align 8
  %9 = load i8*, i8** %3, align 8
  %10 = load i8*, i8** %4, align 8
  %11 = call i32 @_vsprintf_l(i8* noundef %10, i8* noundef %9, %struct.__crt_locale_pointers* noundef null, i8* noundef %8)
  store i32 %11, i32* %5, align 4
  %12 = bitcast i8** %6 to i8*
  call void @llvm.va_end(i8* %12)
  %13 = load i32, i32* %5, align 4
  ret i32 %13
}

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local i32 @vsprintf(i8* noundef %0, i8* noundef %1, i8* noundef %2) #0 comdat {
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i8*, align 8
  store i8* %2, i8** %4, align 8
  store i8* %1, i8** %5, align 8
  store i8* %0, i8** %6, align 8
  %7 = load i8*, i8** %4, align 8
  %8 = load i8*, i8** %5, align 8
  %9 = load i8*, i8** %6, align 8
  %10 = call i32 @_vsnprintf_l(i8* noundef %9, i64 noundef -1, i8* noundef %8, %struct.__crt_locale_pointers* noundef null, i8* noundef %7)
  ret i32 %10
}

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local i32 @_snprintf(i8* noundef %0, i64 noundef %1, i8* noundef %2, ...) #0 comdat {
  %4 = alloca i8*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i8*, align 8
  %7 = alloca i32, align 4
  %8 = alloca i8*, align 8
  store i8* %2, i8** %4, align 8
  store i64 %1, i64* %5, align 8
  store i8* %0, i8** %6, align 8
  %9 = bitcast i8** %8 to i8*
  call void @llvm.va_start(i8* %9)
  %10 = load i8*, i8** %8, align 8
  %11 = load i8*, i8** %4, align 8
  %12 = load i64, i64* %5, align 8
  %13 = load i8*, i8** %6, align 8
  %14 = call i32 @_vsnprintf(i8* noundef %13, i64 noundef %12, i8* noundef %11, i8* noundef %10)
  store i32 %14, i32* %7, align 4
  %15 = bitcast i8** %8 to i8*
  call void @llvm.va_end(i8* %15)
  %16 = load i32, i32* %7, align 4
  ret i32 %16
}

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local i32 @_vsnprintf(i8* noundef %0, i64 noundef %1, i8* noundef %2, i8* noundef %3) #0 comdat {
  %5 = alloca i8*, align 8
  %6 = alloca i8*, align 8
  %7 = alloca i64, align 8
  %8 = alloca i8*, align 8
  store i8* %3, i8** %5, align 8
  store i8* %2, i8** %6, align 8
  store i64 %1, i64* %7, align 8
  store i8* %0, i8** %8, align 8
  %9 = load i8*, i8** %5, align 8
  %10 = load i8*, i8** %6, align 8
  %11 = load i64, i64* %7, align 8
  %12 = load i8*, i8** %8, align 8
  %13 = call i32 @_vsnprintf_l(i8* noundef %12, i64 noundef %11, i8* noundef %10, %struct.__crt_locale_pointers* noundef null, i8* noundef %9)
  ret i32 %13
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @addEdge(i32 noundef %0, i32 noundef %1, i32 noundef %2) #0 {
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca %struct.EDGE*, align 8
  store i32 %2, i32* %4, align 4
  store i32 %1, i32* %5, align 4
  store i32 %0, i32* %6, align 4
  %8 = call noalias i8* @malloc(i64 noundef 16)
  %9 = bitcast i8* %8 to %struct.EDGE*
  store %struct.EDGE* %9, %struct.EDGE** %7, align 8
  %10 = load i32, i32* %5, align 4
  %11 = load %struct.EDGE*, %struct.EDGE** %7, align 8
  %12 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %11, i32 0, i32 0
  store i32 %10, i32* %12, align 8
  %13 = load i32, i32* %4, align 4
  %14 = load %struct.EDGE*, %struct.EDGE** %7, align 8
  %15 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %14, i32 0, i32 1
  store i32 %13, i32* %15, align 4
  %16 = load i32, i32* %6, align 4
  %17 = sext i32 %16 to i64
  %18 = getelementptr inbounds [100010 x %struct.EDGE], [100010 x %struct.EDGE]* @edge, i64 0, i64 %17
  %19 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %18, i32 0, i32 2
  %20 = load %struct.EDGE*, %struct.EDGE** %19, align 8
  %21 = load %struct.EDGE*, %struct.EDGE** %7, align 8
  %22 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %21, i32 0, i32 2
  store %struct.EDGE* %20, %struct.EDGE** %22, align 8
  %23 = load %struct.EDGE*, %struct.EDGE** %7, align 8
  %24 = load i32, i32* %6, align 4
  %25 = sext i32 %24 to i64
  %26 = getelementptr inbounds [100010 x %struct.EDGE], [100010 x %struct.EDGE]* @edge, i64 0, i64 %25
  %27 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %26, i32 0, i32 2
  store %struct.EDGE* %23, %struct.EDGE** %27, align 8
  ret void
}

declare dso_local noalias i8* @malloc(i64 noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @swap(%struct.NODE* noundef %0, %struct.NODE* noundef %1) #0 {
  %3 = alloca %struct.NODE*, align 8
  %4 = alloca %struct.NODE*, align 8
  %5 = alloca %struct.NODE, align 4
  store %struct.NODE* %1, %struct.NODE** %3, align 8
  store %struct.NODE* %0, %struct.NODE** %4, align 8
  %6 = load %struct.NODE*, %struct.NODE** %4, align 8
  %7 = bitcast %struct.NODE* %5 to i8*
  %8 = bitcast %struct.NODE* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %7, i8* align 4 %8, i64 8, i1 false)
  %9 = load %struct.NODE*, %struct.NODE** %4, align 8
  %10 = load %struct.NODE*, %struct.NODE** %3, align 8
  %11 = bitcast %struct.NODE* %9 to i8*
  %12 = bitcast %struct.NODE* %10 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %11, i8* align 4 %12, i64 8, i1 false)
  %13 = load %struct.NODE*, %struct.NODE** %3, align 8
  %14 = bitcast %struct.NODE* %13 to i8*
  %15 = bitcast %struct.NODE* %5 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %14, i8* align 4 %15, i64 8, i1 false)
  ret void
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @push(i32 noundef %0, i32 noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca %struct.NODE, align 4
  %6 = alloca i32, align 4
  store i32 %1, i32* %3, align 4
  store i32 %0, i32* %4, align 4
  %7 = getelementptr inbounds %struct.NODE, %struct.NODE* %5, i32 0, i32 0
  %8 = load i32, i32* %4, align 4
  store i32 %8, i32* %7, align 4
  %9 = getelementptr inbounds %struct.NODE, %struct.NODE* %5, i32 0, i32 1
  %10 = load i32, i32* %3, align 4
  store i32 %10, i32* %9, align 4
  %11 = load i32, i32* @heapSize, align 4
  %12 = add nsw i32 %11, 1
  store i32 %12, i32* @heapSize, align 4
  %13 = load i32, i32* @heapSize, align 4
  %14 = sext i32 %13 to i64
  %15 = getelementptr inbounds [100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 %14
  %16 = bitcast %struct.NODE* %15 to i8*
  %17 = bitcast %struct.NODE* %5 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %16, i8* align 4 %17, i64 8, i1 false)
  %18 = load i32, i32* @heapSize, align 4
  store i32 %18, i32* %6, align 4
  br label %19

19:                                               ; preds = %38, %2
  %20 = load i32, i32* %6, align 4
  %21 = ashr i32 %20, 1
  %22 = icmp sgt i32 %21, 0
  br i1 %22, label %23, label %36

23:                                               ; preds = %19
  %24 = load i32, i32* %6, align 4
  %25 = ashr i32 %24, 1
  %26 = sext i32 %25 to i64
  %27 = getelementptr inbounds [100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 %26
  %28 = getelementptr inbounds %struct.NODE, %struct.NODE* %27, i32 0, i32 1
  %29 = load i32, i32* %28, align 4
  %30 = load i32, i32* %6, align 4
  %31 = sext i32 %30 to i64
  %32 = getelementptr inbounds [100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 %31
  %33 = getelementptr inbounds %struct.NODE, %struct.NODE* %32, i32 0, i32 1
  %34 = load i32, i32* %33, align 4
  %35 = icmp sgt i32 %29, %34
  br label %36

36:                                               ; preds = %23, %19
  %37 = phi i1 [ false, %19 ], [ %35, %23 ]
  br i1 %37, label %38, label %48

38:                                               ; preds = %36
  %39 = load i32, i32* %6, align 4
  %40 = sext i32 %39 to i64
  %41 = getelementptr inbounds %struct.NODE, %struct.NODE* getelementptr inbounds ([100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 0), i64 %40
  %42 = load i32, i32* %6, align 4
  %43 = ashr i32 %42, 1
  %44 = sext i32 %43 to i64
  %45 = getelementptr inbounds %struct.NODE, %struct.NODE* getelementptr inbounds ([100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 0), i64 %44
  call void @swap(%struct.NODE* noundef %45, %struct.NODE* noundef %41)
  %46 = load i32, i32* %6, align 4
  %47 = ashr i32 %46, 1
  store i32 %47, i32* %6, align 4
  br label %19, !llvm.loop !4

48:                                               ; preds = %36
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @get() #0 {
  %1 = alloca %struct.NODE, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = bitcast %struct.NODE* %1 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %4, i8* align 8 bitcast (%struct.NODE* getelementptr inbounds ([100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 1) to i8*), i64 8, i1 false)
  %5 = load i32, i32* @heapSize, align 4
  %6 = sext i32 %5 to i64
  %7 = getelementptr inbounds [100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 %6
  %8 = bitcast %struct.NODE* %7 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 bitcast (%struct.NODE* getelementptr inbounds ([100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 1) to i8*), i8* align 8 %8, i64 8, i1 false)
  %9 = load i32, i32* @heapSize, align 4
  %10 = add nsw i32 %9, -1
  store i32 %10, i32* @heapSize, align 4
  store i32 1, i32* %2, align 4
  br label %11

11:                                               ; preds = %52, %0
  %12 = load i32, i32* %2, align 4
  %13 = shl i32 %12, 1
  %14 = load i32, i32* @heapSize, align 4
  %15 = icmp sle i32 %13, %14
  br i1 %15, label %16, label %60

16:                                               ; preds = %11
  %17 = load i32, i32* %2, align 4
  %18 = shl i32 %17, 1
  store i32 %18, i32* %3, align 4
  %19 = load i32, i32* %3, align 4
  %20 = add nsw i32 %19, 1
  %21 = load i32, i32* @heapSize, align 4
  %22 = icmp sle i32 %20, %21
  br i1 %22, label %23, label %39

23:                                               ; preds = %16
  %24 = load i32, i32* %3, align 4
  %25 = add nsw i32 %24, 1
  %26 = sext i32 %25 to i64
  %27 = getelementptr inbounds [100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 %26
  %28 = getelementptr inbounds %struct.NODE, %struct.NODE* %27, i32 0, i32 1
  %29 = load i32, i32* %28, align 4
  %30 = load i32, i32* %3, align 4
  %31 = sext i32 %30 to i64
  %32 = getelementptr inbounds [100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 %31
  %33 = getelementptr inbounds %struct.NODE, %struct.NODE* %32, i32 0, i32 1
  %34 = load i32, i32* %33, align 4
  %35 = icmp slt i32 %29, %34
  br i1 %35, label %36, label %39

36:                                               ; preds = %23
  %37 = load i32, i32* %3, align 4
  %38 = add nsw i32 %37, 1
  store i32 %38, i32* %3, align 4
  br label %39

39:                                               ; preds = %36, %23, %16
  %40 = load i32, i32* %2, align 4
  %41 = sext i32 %40 to i64
  %42 = getelementptr inbounds [100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 %41
  %43 = getelementptr inbounds %struct.NODE, %struct.NODE* %42, i32 0, i32 1
  %44 = load i32, i32* %43, align 4
  %45 = load i32, i32* %3, align 4
  %46 = sext i32 %45 to i64
  %47 = getelementptr inbounds [100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 %46
  %48 = getelementptr inbounds %struct.NODE, %struct.NODE* %47, i32 0, i32 1
  %49 = load i32, i32* %48, align 4
  %50 = icmp sle i32 %44, %49
  br i1 %50, label %51, label %52

51:                                               ; preds = %39
  br label %60

52:                                               ; preds = %39
  %53 = load i32, i32* %3, align 4
  %54 = sext i32 %53 to i64
  %55 = getelementptr inbounds %struct.NODE, %struct.NODE* getelementptr inbounds ([100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 0), i64 %54
  %56 = load i32, i32* %2, align 4
  %57 = sext i32 %56 to i64
  %58 = getelementptr inbounds %struct.NODE, %struct.NODE* getelementptr inbounds ([100010 x %struct.NODE], [100010 x %struct.NODE]* @heap, i64 0, i64 0), i64 %57
  call void @swap(%struct.NODE* noundef %58, %struct.NODE* noundef %55)
  %59 = load i32, i32* %3, align 4
  store i32 %59, i32* %2, align 4
  br label %11, !llvm.loop !6

60:                                               ; preds = %51, %11
  %61 = bitcast %struct.NODE* %1 to i64*
  %62 = load i64, i64* %61, align 4
  ret i64 %62
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca %struct.NODE, align 4
  %8 = alloca %struct.NODE, align 4
  %9 = alloca %struct.EDGE*, align 8
  %10 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %11 = call i32 (i8*, ...) @scanf(i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @"??_C@_06OOABJBHO@?$CFd?$CFd?$CFd?$AA@", i64 0, i64 0), i32* noundef @n, i32* noundef @m, i32* noundef @start)
  store i32 0, i32* %2, align 4
  br label %12

12:                                               ; preds = %21, %0
  %13 = load i32, i32* %2, align 4
  %14 = load i32, i32* @m, align 4
  %15 = icmp slt i32 %13, %14
  br i1 %15, label %16, label %24

16:                                               ; preds = %12
  %17 = call i32 (i8*, ...) @scanf(i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @"??_C@_06OOABJBHO@?$CFd?$CFd?$CFd?$AA@", i64 0, i64 0), i32* noundef %3, i32* noundef %4, i32* noundef %5)
  %18 = load i32, i32* %5, align 4
  %19 = load i32, i32* %4, align 4
  %20 = load i32, i32* %3, align 4
  call void @addEdge(i32 noundef %20, i32 noundef %19, i32 noundef %18)
  br label %21

21:                                               ; preds = %16
  %22 = load i32, i32* %2, align 4
  %23 = add nsw i32 %22, 1
  store i32 %23, i32* %2, align 4
  br label %12, !llvm.loop !7

24:                                               ; preds = %12
  store i32 1, i32* %6, align 4
  br label %25

25:                                               ; preds = %33, %24
  %26 = load i32, i32* %6, align 4
  %27 = load i32, i32* @n, align 4
  %28 = icmp sle i32 %26, %27
  br i1 %28, label %29, label %36

29:                                               ; preds = %25
  %30 = load i32, i32* %6, align 4
  %31 = sext i32 %30 to i64
  %32 = getelementptr inbounds [100010 x i32], [100010 x i32]* @dis, i64 0, i64 %31
  store i32 1000000001, i32* %32, align 4
  br label %33

33:                                               ; preds = %29
  %34 = load i32, i32* %6, align 4
  %35 = add nsw i32 %34, 1
  store i32 %35, i32* %6, align 4
  br label %25, !llvm.loop !8

36:                                               ; preds = %25
  %37 = load i32, i32* @start, align 4
  %38 = sext i32 %37 to i64
  %39 = getelementptr inbounds [100010 x i32], [100010 x i32]* @dis, i64 0, i64 %38
  store i32 0, i32* %39, align 4
  %40 = load i32, i32* @start, align 4
  call void @push(i32 noundef %40, i32 noundef 0)
  br label %41

41:                                               ; preds = %126, %36
  %42 = load i32, i32* @heapSize, align 4
  %43 = icmp sgt i32 %42, 0
  br i1 %43, label %44, label %127

44:                                               ; preds = %41
  %45 = call i64 @get()
  %46 = bitcast %struct.NODE* %7 to i64*
  store i64 %45, i64* %46, align 4
  br label %47

47:                                               ; preds = %59, %44
  %48 = load i32, i32* @heapSize, align 4
  %49 = icmp sgt i32 %48, 0
  br i1 %49, label %50, label %57

50:                                               ; preds = %47
  %51 = getelementptr inbounds %struct.NODE, %struct.NODE* %7, i32 0, i32 0
  %52 = load i32, i32* %51, align 4
  %53 = sext i32 %52 to i64
  %54 = getelementptr inbounds [100010 x i32], [100010 x i32]* @sure, i64 0, i64 %53
  %55 = load i32, i32* %54, align 4
  %56 = icmp ne i32 %55, 0
  br label %57

57:                                               ; preds = %50, %47
  %58 = phi i1 [ false, %47 ], [ %56, %50 ]
  br i1 %58, label %59, label %64

59:                                               ; preds = %57
  %60 = call i64 @get()
  %61 = bitcast %struct.NODE* %8 to i64*
  store i64 %60, i64* %61, align 4
  %62 = bitcast %struct.NODE* %7 to i8*
  %63 = bitcast %struct.NODE* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %62, i8* align 4 %63, i64 8, i1 false)
  br label %47, !llvm.loop !9

64:                                               ; preds = %57
  %65 = getelementptr inbounds %struct.NODE, %struct.NODE* %7, i32 0, i32 0
  %66 = load i32, i32* %65, align 4
  %67 = sext i32 %66 to i64
  %68 = getelementptr inbounds [100010 x i32], [100010 x i32]* @sure, i64 0, i64 %67
  %69 = load i32, i32* %68, align 4
  %70 = icmp ne i32 %69, 0
  br i1 %70, label %71, label %72

71:                                               ; preds = %64
  br label %127

72:                                               ; preds = %64
  %73 = getelementptr inbounds %struct.NODE, %struct.NODE* %7, i32 0, i32 0
  %74 = load i32, i32* %73, align 4
  %75 = sext i32 %74 to i64
  %76 = getelementptr inbounds [100010 x i32], [100010 x i32]* @sure, i64 0, i64 %75
  store i32 1, i32* %76, align 4
  %77 = getelementptr inbounds %struct.NODE, %struct.NODE* %7, i32 0, i32 0
  %78 = load i32, i32* %77, align 4
  %79 = sext i32 %78 to i64
  %80 = getelementptr inbounds [100010 x %struct.EDGE], [100010 x %struct.EDGE]* @edge, i64 0, i64 %79
  %81 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %80, i32 0, i32 2
  %82 = load %struct.EDGE*, %struct.EDGE** %81, align 8
  store %struct.EDGE* %82, %struct.EDGE** %9, align 8
  br label %83

83:                                               ; preds = %122, %72
  %84 = load %struct.EDGE*, %struct.EDGE** %9, align 8
  %85 = icmp ne %struct.EDGE* %84, null
  br i1 %85, label %86, label %126

86:                                               ; preds = %83
  %87 = getelementptr inbounds %struct.NODE, %struct.NODE* %7, i32 0, i32 1
  %88 = load i32, i32* %87, align 4
  %89 = load %struct.EDGE*, %struct.EDGE** %9, align 8
  %90 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %89, i32 0, i32 1
  %91 = load i32, i32* %90, align 4
  %92 = add nsw i32 %88, %91
  %93 = load %struct.EDGE*, %struct.EDGE** %9, align 8
  %94 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %93, i32 0, i32 0
  %95 = load i32, i32* %94, align 8
  %96 = sext i32 %95 to i64
  %97 = getelementptr inbounds [100010 x i32], [100010 x i32]* @dis, i64 0, i64 %96
  %98 = load i32, i32* %97, align 4
  %99 = icmp slt i32 %92, %98
  br i1 %99, label %100, label %121

100:                                              ; preds = %86
  %101 = getelementptr inbounds %struct.NODE, %struct.NODE* %7, i32 0, i32 1
  %102 = load i32, i32* %101, align 4
  %103 = load %struct.EDGE*, %struct.EDGE** %9, align 8
  %104 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %103, i32 0, i32 1
  %105 = load i32, i32* %104, align 4
  %106 = add nsw i32 %102, %105
  %107 = load %struct.EDGE*, %struct.EDGE** %9, align 8
  %108 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %107, i32 0, i32 0
  %109 = load i32, i32* %108, align 8
  %110 = sext i32 %109 to i64
  %111 = getelementptr inbounds [100010 x i32], [100010 x i32]* @dis, i64 0, i64 %110
  store i32 %106, i32* %111, align 4
  %112 = load %struct.EDGE*, %struct.EDGE** %9, align 8
  %113 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %112, i32 0, i32 0
  %114 = load i32, i32* %113, align 8
  %115 = sext i32 %114 to i64
  %116 = getelementptr inbounds [100010 x i32], [100010 x i32]* @dis, i64 0, i64 %115
  %117 = load i32, i32* %116, align 4
  %118 = load %struct.EDGE*, %struct.EDGE** %9, align 8
  %119 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %118, i32 0, i32 0
  %120 = load i32, i32* %119, align 8
  call void @push(i32 noundef %120, i32 noundef %117)
  br label %121

121:                                              ; preds = %100, %86
  br label %122

122:                                              ; preds = %121
  %123 = load %struct.EDGE*, %struct.EDGE** %9, align 8
  %124 = getelementptr inbounds %struct.EDGE, %struct.EDGE* %123, i32 0, i32 2
  %125 = load %struct.EDGE*, %struct.EDGE** %124, align 8
  store %struct.EDGE* %125, %struct.EDGE** %9, align 8
  br label %83, !llvm.loop !10

126:                                              ; preds = %83
  br label %41, !llvm.loop !11

127:                                              ; preds = %71, %41
  store i32 1, i32* %10, align 4
  br label %128

128:                                              ; preds = %138, %127
  %129 = load i32, i32* %10, align 4
  %130 = load i32, i32* @n, align 4
  %131 = icmp sle i32 %129, %130
  br i1 %131, label %132, label %141

132:                                              ; preds = %128
  %133 = load i32, i32* %10, align 4
  %134 = sext i32 %133 to i64
  %135 = getelementptr inbounds [100010 x i32], [100010 x i32]* @dis, i64 0, i64 %134
  %136 = load i32, i32* %135, align 4
  %137 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @"??_C@_03JDANDILB@?$CFd?5?$AA@", i64 0, i64 0), i32 noundef %136)
  br label %138

138:                                              ; preds = %132
  %139 = load i32, i32* %10, align 4
  %140 = add nsw i32 %139, 1
  store i32 %140, i32* %10, align 4
  br label %128, !llvm.loop !12

141:                                              ; preds = %128
  ret i32 0
}

; Function Attrs: noinline nounwind optnone uwtable
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

; Function Attrs: noinline nounwind optnone uwtable
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

; Function Attrs: nofree nosync nounwind willreturn
declare void @llvm.va_start(i8*) #3

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local i32 @_vsprintf_l(i8* noundef %0, i8* noundef %1, %struct.__crt_locale_pointers* noundef %2, i8* noundef %3) #0 comdat {
  %5 = alloca i8*, align 8
  %6 = alloca %struct.__crt_locale_pointers*, align 8
  %7 = alloca i8*, align 8
  %8 = alloca i8*, align 8
  store i8* %3, i8** %5, align 8
  store %struct.__crt_locale_pointers* %2, %struct.__crt_locale_pointers** %6, align 8
  store i8* %1, i8** %7, align 8
  store i8* %0, i8** %8, align 8
  %9 = load i8*, i8** %5, align 8
  %10 = load %struct.__crt_locale_pointers*, %struct.__crt_locale_pointers** %6, align 8
  %11 = load i8*, i8** %7, align 8
  %12 = load i8*, i8** %8, align 8
  %13 = call i32 @_vsnprintf_l(i8* noundef %12, i64 noundef -1, i8* noundef %11, %struct.__crt_locale_pointers* noundef %10, i8* noundef %9)
  ret i32 %13
}

; Function Attrs: nofree nosync nounwind willreturn
declare void @llvm.va_end(i8*) #3

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local i32 @_vsnprintf_l(i8* noundef %0, i64 noundef %1, i8* noundef %2, %struct.__crt_locale_pointers* noundef %3, i8* noundef %4) #0 comdat {
  %6 = alloca i8*, align 8
  %7 = alloca %struct.__crt_locale_pointers*, align 8
  %8 = alloca i8*, align 8
  %9 = alloca i64, align 8
  %10 = alloca i8*, align 8
  %11 = alloca i32, align 4
  store i8* %4, i8** %6, align 8
  store %struct.__crt_locale_pointers* %3, %struct.__crt_locale_pointers** %7, align 8
  store i8* %2, i8** %8, align 8
  store i64 %1, i64* %9, align 8
  store i8* %0, i8** %10, align 8
  %12 = load i8*, i8** %6, align 8
  %13 = load %struct.__crt_locale_pointers*, %struct.__crt_locale_pointers** %7, align 8
  %14 = load i8*, i8** %8, align 8
  %15 = load i64, i64* %9, align 8
  %16 = load i8*, i8** %10, align 8
  %17 = call i64* @__local_stdio_printf_options()
  %18 = load i64, i64* %17, align 8
  %19 = or i64 %18, 1
  %20 = call i32 @__stdio_common_vsprintf(i64 noundef %19, i8* noundef %16, i64 noundef %15, i8* noundef %14, %struct.__crt_locale_pointers* noundef %13, i8* noundef %12)
  store i32 %20, i32* %11, align 4
  %21 = load i32, i32* %11, align 4
  %22 = icmp slt i32 %21, 0
  br i1 %22, label %23, label %24

23:                                               ; preds = %5
  br label %26

24:                                               ; preds = %5
  %25 = load i32, i32* %11, align 4
  br label %26

26:                                               ; preds = %24, %23
  %27 = phi i32 [ -1, %23 ], [ %25, %24 ]
  ret i32 %27
}

declare dso_local i32 @__stdio_common_vsprintf(i64 noundef, i8* noundef, i64 noundef, i8* noundef, %struct.__crt_locale_pointers* noundef, i8* noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local i64* @__local_stdio_printf_options() #0 comdat {
  ret i64* @__local_stdio_printf_options._OptionsStorage
}

; Function Attrs: noinline nounwind optnone uwtable
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

declare dso_local %struct._iobuf* @__acrt_iob_func(i32 noundef) #1

declare dso_local i32 @__stdio_common_vfscanf(i64 noundef, %struct._iobuf* noundef, i8* noundef, %struct.__crt_locale_pointers* noundef, i8* noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define linkonce_odr dso_local i64* @__local_stdio_scanf_options() #0 comdat {
  ret i64* @__local_stdio_scanf_options._OptionsStorage
}

; Function Attrs: noinline nounwind optnone uwtable
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

declare dso_local i32 @__stdio_common_vfprintf(i64 noundef, %struct._iobuf* noundef, i8* noundef, %struct.__crt_locale_pointers* noundef, i8* noundef) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { argmemonly nofree nounwind willreturn }
attributes #3 = { nofree nosync nounwind willreturn }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 2}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 1}
!3 = !{!"clang version 14.0.5"}
!4 = distinct !{!4, !5}
!5 = !{!"llvm.loop.mustprogress"}
!6 = distinct !{!6, !5}
!7 = distinct !{!7, !5}
!8 = distinct !{!8, !5}
!9 = distinct !{!9, !5}
!10 = distinct !{!10, !5}
!11 = distinct !{!11, !5}
!12 = distinct !{!12, !5}
