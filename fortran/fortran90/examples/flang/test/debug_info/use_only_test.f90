!RUN: %flang -g -S -emit-llvm %s_mod.f90 %s
!RUN: cat use_only_test.ll | FileCheck %s

!CHECK: [[MOD_VAR:![0-9]+]] = distinct !DIGlobalVariable(name: "mod_var1"
!CHECK: distinct !DICompileUnit
!CHECK-SAME: imports: [[IMPLIST:![0-9]+]]
!CHECK: [[IMPLIST]] = !{[[IMPORT:![0-9]+]]
!CHECK: [[IMPORT]] = !DIImportedEntity(tag: DW_TAG_imported_declaration
!CHECK-SAME: entity: [[MOD_VAR]]
!CHECK-NOT: !DIImportedEntity(tag: DW_TAG_imported_module

program main
  use use_only_mod, only: mod_var1
  implicit none
  print*, mod_var1
end
