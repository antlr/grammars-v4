#pragma author WerWolv
#pragma description Java Class

#pragma endian big
#pragma pattern_limit 100000000
#pragma MIME application/x-java-applet

import std.string;
import std.math;
import std.core;
import std.io;
import std.sys;

using BitfieldOrder = std::core::BitfieldOrder;

using u1 = u8;
using u2 = u16;
using u4 = u32;

using f4 = float;
using f8 = double;

using cp_ref = u16 [[format("fmt::const_ref_top")]];

bool applyPadding = false;

struct cp_info {
    bool isTop = false;
    if(applyPadding) {
        isTop = true;
        applyPadding = false;
    }
    else {
        u1 tag;
        match(tag) {
            (1): { /* cp_utf8 */
                u2 length;
                char bytes[length];
            }
            (3): { /* cp_integer */
                u4 bytes;
            }
            (4): { /* cp_float */
                float bytes;
            }
            (5): { /* cp_long */
                u64 bytes;
                applyPadding = true;
            }
            (6): { /* cp_double */
                double bytes;
                applyPadding = true;
            }
            (7): { /* cp_class */
                cp_ref name_index;
            }
            (8): { /* cp_string */
                cp_ref string_index;
            }
            (9): { /* cp_fieldref */
                cp_ref class_index;
                cp_ref name_and_type_index;
            }
            (10): { /* cp_methodref */
                cp_ref class_index;
                cp_ref name_and_type_index;
            }
            (11): { /* cp_interface_methodref */
                cp_ref class_index;
                cp_ref name_and_type_index;
            }
            (12): { /* cp_name_and_type */
                cp_ref name_index;
                cp_ref descriptor_index;
            }
            (15): { /* cp_method_handle */
                u1 reference_kind;
                cp_ref reference_index;
            }
            (16): { /* cp_method_type */
                cp_ref descriptor_index;
            }
            (18): { /* cp_invoke_dynamic */
                u2 bootstrap_method_attr_index;
                cp_ref name_and_type_index;
            }
            (19): { /* cp_module */
                cp_ref name_index;
            }
            (20): { /* cp_package */
                cp_ref name_index;
            }
        }
    }
} [[format("fmt::const_name")]];

namespace fmt {
    fn const_name(auto tag) {
        if(tag.isTop) return "Top";
        match(tag.tag) {
            (1): return "Utf8";
            (3): return "Integer";
            (4): return "Float";
            (5): return "Long";
            (6): return "Double";
            (7): return "Class";
            (8): return "String";
            (9): return "Fieldref";
            (10): return "Methodref";
            (11): return "InterfaceMethodref";
            (12): return "NameAndType";
            (15): return "MethodHandle";
            (16): return "MethodType";
            (18): return "InvokeDynamic";
            (_): return "Unknown";
        }
    };

    fn const_ref(u2 index) {
        cp_info info = file.constant_pool[index-1];
        match(info.tag) {
            (1): return info.bytes;
            (3): return std::format("{:d} [{:d}]", index, info.bytes);
            (4): return std::format("{:d} [{:f}]", index, info.bytes);
            (5): return std::format("{:d} [{:d}", index, info.bytes);
            (6): return std::format("{:d} [{:f}", index, info.bytes);
            (7): return std::format("{:d} [Class({})]", index, fmt::const_ref(info.name_index));
            (8): return std::format("{:d} [String({})]", index, fmt::const_ref(info.string_index));
            (9): return std::format("{:d} [Fieldref({}, {})]", index, fmt::const_ref(info.class_index), fmt::const_ref(info.name_and_type_index));
            (10): return std::format("{:d} [Methodref({}, {})]", index, fmt::const_ref(info.class_index), fmt::const_ref(info.name_and_type_index));
            (11): return std::format("{:d} [InterfaceMethodref({}, {})]", index, fmt::const_ref(info.class_index), fmt::const_ref(info.name_and_type_index));
            (12): return std::format("{:d} [NameAndType({}, {})]", index, fmt::const_ref(info.name_index), fmt::const_ref(info.descriptor_index));
            (15): return std::format("{:d} [MethodHandle({}, {})]", index, info.reference_kind, fmt::const_ref(info.reference_index));
            (16): return std::format("{:d} [MethodType({})]", index, fmt::const_ref(info.descriptor_index));
            (18): return std::format("{:d} [InvokeDynamic({}, {})]", index, info.bootstrap_method_attr_index, fmt::const_ref(info.name_and_type_index));
            (19): return std::format("{:d} [Module({})]", index, fmt::const_ref(info.name_index));
            (20): return std::format("{:d} [Package({})]", index, fmt::const_ref(info.name_index));
            (_): return std::format("{:d} [Unknown]", index);
        }
    };

    fn const_ref_top(u2 index) {
        cp_info info = file.constant_pool[index-1];
        match(info.tag) {
            (1): return std::format("{:d} [{:s}]", index, info.bytes);
            (_): return fmt::const_ref(index);
        }
    };

    fn byte_code_fmt(auto value) {
        match(value) {
            (00): return "nop";
            (01): return "aconst_null";
            (02): return "iconst_m1";
            (03): return "iconst_0";
            (04): return "iconst_1";
            (05): return "iconst_2";
            (06): return "iconst_3";
            (07): return "iconst_4";
            (08): return "iconst_5";
            (09): return "lconst_0";
            (10): return "lconst_1";
            (11): return "fconst_0";
            (12): return "fconst_1";
            (13): return "fconst_2";
            (14): return "dconst_0";
            (15): return "dconst_1";
            (16): return "bipush";
            (17): return "sipush";
            (18): return "ldc";
            (19): return "ldc_w";
            (20): return "ldc2_w";
            (21): return "iload";
            (22): return "lload";
            (23): return "fload";
            (24): return "dload";
            (25): return "aload";
            (26): return "iload_0";
            (27): return "iload_1";
            (28): return "iload_2";
            (29): return "iload_3";
            (30): return "lload_0";
            (31): return "lload_1";
            (32): return "lload_2";
            (33): return "lload_3";
            (34): return "fload_0";
            (35): return "fload_1";
            (36): return "fload_2";
            (37): return "fload_3";
            (38): return "dload_0";
            (39): return "dload_1";
            (40): return "dload_2";
            (41): return "dload_3";
            (42): return "aload_0";
            (43): return "aload_1";
            (44): return "aload_2";
            (45): return "aload_3";
            (46): return "iaload";
            (47): return "laload";
            (48): return "faload";
            (49): return "daload";
            (50): return "aaload";
            (51): return "baload";
            (52): return "caload";
            (53): return "saload";
            (54): return "istore";
            (55): return "lstore";
            (56): return "fstore";
            (57): return "dstore";
            (58): return "astore";
            (59): return "istore_0";
            (60): return "istore_1";
            (61): return "istore_2";
            (62): return "istore_3";
            (63): return "lstore_0";
            (64): return "lstore_1";
            (65): return "lstore_2";
            (66): return "lstore_3";
            (67): return "fstore_0";
            (68): return "fstore_1";
            (69): return "fstore_2";
            (70): return "fstore_3";
            (71): return "dstore_0";
            (72): return "dstore_1";
            (73): return "dstore_2";
            (74): return "dstore_3";
            (75): return "astore_0";
            (76): return "astore_1";
            (77): return "astore_2";
            (78): return "astore_3";
            (79): return "iastore";
            (80): return "lastore";
            (81): return "fastore";
            (82): return "dastore";
            (83): return "aastore";
            (84): return "bastore";
            (85): return "castore";
            (86): return "sastore";
            (87): return "pop";
            (88): return "pop2";
            (89): return "dup";
            (90): return "dup_x1";
            (91): return "dup_x2";
            (92): return "dup2";
            (93): return "dup2_x1";
            (94): return "dup2_x2";
            (95): return "swap";
            (96): return "iadd";
            (97): return "ladd";
            (98): return "fadd";
            (99): return "dadd";
            (100): return "isub";
            (101): return "lsub";
            (102): return "fsub";
            (103): return "dsub";
            (104): return "imul";
            (105): return "lmul";
            (106): return "fmul";
            (107): return "dmul";
            (108): return "idiv";
            (109): return "ldiv";
            (110): return "fdiv";
            (111): return "ddiv";
            (112): return "irem";
            (113): return "lrem";
            (114): return "frem";
            (115): return "drem";
            (116): return "ineg";
            (117): return "lneg";
            (118): return "fneg";
            (119): return "dneg";
            (120): return "ishl";
            (121): return "lshl";
            (122): return "ishr";
            (123): return "lshr";
            (124): return "iushr";
            (125): return "lushr";
            (126): return "iand";
            (127): return "land";
            (128): return "ior";
            (129): return "lor";
            (130): return "ixor";
            (131): return "lxor";
            (132): return "iinc";
            (133): return "i2l";
            (134): return "i2f";
            (135): return "i2d";
            (136): return "l2i";
            (137): return "l2f";
            (138): return "l2d";
            (139): return "f2i";
            (140): return "f2l";
            (141): return "f2d";
            (142): return "d2i";
            (143): return "d2l";
            (144): return "d2f";
            (145): return "i2b";
            (146): return "i2c";
            (147): return "i2s";
            (148): return "lcmp";
            (149): return "fcmpl";
            (150): return "fcmpg";
            (151): return "dcmpl";
            (152): return "dcmpg";
            (153): return "ifeq";
            (154): return "ifne";
            (155): return "iflt";
            (156): return "ifge";
            (157): return "ifgt";
            (158): return "ifle";
            (159): return "if_icmpeq";
            (160): return "if_icmpne";
            (161): return "if_icmplt";
            (162): return "if_icmpge";
            (163): return "if_icmpgt";
            (164): return "if_icmple";
            (165): return "if_acmpeq";
            (166): return "if_acmpne";
            (167): return "goto";
            (168): return "jsr";
            (169): return "ret";
            (170): return "tableswitch";
            (171): return "lookupswitch";
            (172): return "ireturn";
            (173): return "lreturn";
            (174): return "freturn";
            (175): return "dreturn";
            (176): return "areturn";
            (177): return "return";
            (178): return "getstatic";
            (179): return "putstatic";
            (180): return "getfield";
            (181): return "putfield";
            (182): return "invokevirtual";
            (183): return "invokespecial";
            (184): return "invokestatic";
            (185): return "invokeinterface";
            (186): return "invokedynamic";
            (187): return "new";
            (188): return "newarray";
            (189): return "anewarray";
            (190): return "arraylength";
            (191): return "athrow";
            (192): return "checkcast";
            (193): return "instanceof";
            (194): return "monitorenter";
            (195): return "monitorexit";
            (196): return "wide";
            (197): return "multianewarray";
            (198): return "ifnull";
            (199): return "ifnonnull";
            (200): return "goto_w";
            (201): return "jsr_w";
            (202): return "breakpoint";
            (254): return "impdep1";
            (255): return "impdep2";
            (_): return std::format("{:d} [Unknown]", value);
        }
    };

    fn atype_fmt(auto atype) {
        match(atype) {
            (4): return "T_BOOLEAN";
            (5): return "T_CHAR";
            (6): return "T_FLOAT";
            (7): return "T_DOUBLE";
            (8): return "T_BYTE";
            (9): return "T_SHORT";
            (10): return "T_INT";
            (11): return "T_LONG";
        }
    };

    fn instruction_code_fmt(ref auto code) {
        return fmt::byte_code_fmt(code.mnemonic);
    };

    fn attribute(auto info) {
        return file.constant_pool[info.attribute_name_index-1].bytes;
    };

    fn verif_type_info(auto info) {
        match(info.tag) {
            (0): return "TOP";
            (1): return "INTEGER";
            (2): return "FLOAT";
            (3): return "DOUBLE";
            (4): return "LONG";
            (5): return "NULL";
            (6): return "UNINITIALIZED_THIS";
            (7): return std::format("Object({})", fmt::const_ref(info.cpool_index));
            (8): return std::format("Uninitialized({})", info.offset);
            (_): return "Unknown";
        }
    };

    fn stack_map_frame(auto frame) {
        match(frame.frame_type) {
            (0 ... 63): return std::format("SAME [{}]", frame.frame_type);
            (64 ... 127): return std::format("SAME_LOCALS_1_STACK_ITEM [{}]", frame.frame_type);
            (247): return std::format("SAME_LOCALS_1_STACK_ITEM_EXTENDED [{}]", frame.frame_type);
            (248 ... 250): return std::format("CHOP [{}]", frame.frame_type);
            (251): return std::format("SAME_FRAME_EXTENDED [{}]", frame.frame_type);
            (252 ... 254): return std::format("APPEND [{}]", frame.frame_type);
            (255): return std::format("FULL_FRAME [{}]", frame.frame_type);
        }
    };

    fn element_value(auto element) {
        match(element.tag) {
            ('B'): return std::format("byte [{:s}]", fmt::const_ref(element.const_value_index));
            ('C'): return std::format("char [{:s}]", fmt::const_ref(element.const_value_index));
            ('D'): return std::format("double [{:s}]", fmt::const_ref(element.const_value_index));
            ('F'): return std::format("float [{:s}]", fmt::const_ref(element.const_value_index));
            ('I'): return std::format("int [{:s}]", fmt::const_ref(element.const_value_index));
            ('J'): return std::format("long [{:s}]", fmt::const_ref(element.const_value_index));
            ('S'): return std::format("short [{:s}]", fmt::const_ref(element.const_value_index));
            ('Z'): return std::format("boolean [{:s}]", fmt::const_ref(element.const_value_index));
            ('s'): return std::format("String [{:s}]", fmt::const_ref(element.const_value_index));
            ('e'): return std::format("Enum [{:s}, {:s}]", fmt::const_ref(element.type_name_index), fmt::const_ref(element.const_name_index));
            ('c'): return std::format("Class [{:s}]", fmt::const_ref(element.class_info_index));
            ('@'): return std::format("Annotation [{:s}]", fmt::const_ref(element.annotation_value.type_index));
            ('['): return std::format("Array [{:s}]", fmt::const_ref(element.num_values));
        }
    };
}

enum major_version : u2 {
    JDK_1_1              = 45,
    JDK_1_2              = 46,
    JDK_1_3              = 47,
    JDK_1_4              = 48,

    Java_SE_5_0          = 49,
    Java_SE_6            = 50,
    Java_SE_7            = 51,
    Java_SE_8            = 52,
    Java_SE_9            = 53,
    Java_SE_10           = 54,
    Java_SE_11           = 55,
    Java_SE_12           = 56,
    Java_SE_13           = 57,
    Java_SE_14           = 58,
    Java_SE_15           = 59,
    Java_SE_16           = 60,
    Java_SE_17           = 61,
};

bitfield access_flags_method  {
    ACC_PUBLIC      : 1;    // 0x0001
    ACC_PRIVATE     : 1;    // 0x0002
    ACC_PROTECTED   : 1;    // 0x0004
    ACC_STATIC      : 1;    // 0x0008
    ACC_FINAL       : 1;    // 0x0010
    ACC_SYNCHRONIZED: 1;    // 0x0020
    ACC_BRIDGE      : 1;    // 0x0040
    ACC_VARARGS     : 1;    // 0x0080
    ACC_NATIVE      : 1;    // 0x0100
    padding         : 1;    // 0x0200
    ACC_ABSTRACT    : 1;    // 0x0400
    ACC_STRICT      : 1;    // 0x0800
    ACC_SYNTHETIC   : 1;    // 0x1000
    padding         : 1;    // 0x2000
    padding         : 1;    // 0x4000
    padding         : 1;    // 0x8000
} [[bitfield_order(BitfieldOrder::LeastToMostSignificant, 16)]];

bitfield access_flags_field  {
    ACC_PUBLIC      : 1;    // 0x0001
    ACC_PRIVATE     : 1;    // 0x0002
    ACC_PROTECTED   : 1;    // 0x0004
    ACC_STATIC      : 1;    // 0x0008
    ACC_FINAL       : 1;    // 0x0010
    padding         : 1;    // 0x0020
    ACC_VOLATILE    : 1;    // 0x0040
    ACC_TRANSIENT   : 1;    // 0x0080
    padding         : 1;    // 0x0100
    padding         : 1;    // 0x0200
    padding         : 1;    // 0x0400
    padding         : 1;    // 0x0800
    ACC_SYNTHETIC   : 1;    // 0x1000
    padding         : 1;    // 0x2000
    ACC_ENUM        : 1;    // 0x4000
    padding         : 1;    // 0x8000
} [[bitfield_order(BitfieldOrder::LeastToMostSignificant, 16)]];

bitfield access_flags_class  {
    ACC_PUBLIC       : 1;    // 0x0001
    padding          : 1;    // 0x0002
    padding          : 1;    // 0x0004
    padding          : 1;    // 0x0008
    ACC_FINAL        : 1;    // 0x0010
    ACC_SUPER        : 1;    // 0x0020
    padding          : 1;    // 0x0040
    padding          : 1;    // 0x0080
    padding          : 1;    // 0x0100
    ACC_INTERFACE    : 1;    // 0x0200
    ACC_ABSTRACT     : 1;    // 0x0400
    padding          : 1;    // 0x0800
    ACC_SYNTHETIC    : 1;    // 0x1000
    ACC_ANNOTATION   : 1;    // 0x2000
    ACC_ENUM         : 1;    // 0x4000
    ACC_MODULE       : 1;    // 0x8000
} [[bitfield_order(BitfieldOrder::LeastToMostSignificant, 16)]];

bitfield access_flags_module {
    padding          : 1;    // 0x0001
    padding          : 1;    // 0x0002
    padding          : 1;    // 0x0004
    padding          : 1;    // 0x0008
    padding          : 1;    // 0x0010
    ACC_TRANSITIVE   : 1;    // 0x0020
    ACC_STATIC_PHASE : 1;    // 0x0040
    padding          : 1;    // 0x0080
    padding          : 1;    // 0x0100
    padding          : 1;    // 0x0200
    padding          : 1;    // 0x0400
    padding          : 1;    // 0x0800
    ACC_SYNTHETIC    : 1;    // 0x1000
    padding          : 1;    // 0x2000
    padding          : 1;    // 0x4000
    ACC_MANDATED     : 1;    // 0x8000
} [[bitfield_order(BitfieldOrder::LeastToMostSignificant, 16)]];

using attribute_code;

struct attribute_constant_value {
    cp_ref constantvalue_index;
} [[static]];


struct verification_type_info {
    u1 tag;
    match(tag) {
        (0): { /* Top_variable_info */
        }
        (1): { /* Integer_variable_info */
        }
        (2): { /* Float_variable_info */
        }
        (3): { /* Long_variable_info */
        }
        (4): { /* Double_variable_info */
        }
        (5): { /* Null_variable_info */
        }
        (6): { /* UninitializedThis_variable_info */
        }
        (7): { /* Object_variable_info */
            cp_ref cpool_index;
        }
        (8): { /* Uninitialized_variable_info */
            u2 offset;
        }
    }
} [[format("fmt::verif_type_info")]];

struct same_locals_1_stack_item_frame {
    verification_type_info stack[1];
};

struct same_locals_1_stack_item_frame_extended {
    u2 offset_delta;
    verification_type_info stack[1];
};

struct chop_frame {
    u2 offset_delta;
};

struct same_frame_extended {
    u2 offset_delta;
} [[static]];

struct append_frame<auto frame_type> {
    u2 offset_delta;
    verification_type_info locals[frame_type - 251];
};

struct full_frame {
    u2 offset_delta;
    u2 number_of_locals;
    verification_type_info locals[number_of_locals];
    u2 number_of_stack_items;
    verification_type_info stack[number_of_stack_items];
};

struct stack_map_frame {
    u1 frame_type;
    match(frame_type) {
        (0 ... 63): {} /* nothing */
        (64 ... 127): same_locals_1_stack_item_frame child [[inline]];
        (247): same_locals_1_stack_item_frame_extended child [[inline]];
        (248 ... 250): chop_frame child [[inline]];
        (251): same_frame_extended child [[inline]];
        (252 ... 254): append_frame<frame_type> child [[inline]];
        (255): full_frame child [[inline]];
    }
} [[format("fmt::stack_map_frame")]];

struct same_frame {
    u1 frame_type = SAME; /* 0-63 */
};

struct attribute_stack_map_table {
    u2 number_of_entries;
    stack_map_frame entries[number_of_entries];
};

struct attribute_exception {
    u2 number_of_exceptions;
    cp_ref exception_index_table[number_of_exceptions];
};

struct inner_class {
    cp_ref inner_class_info_index;
    u16 potential_outer_class_info_index [[hidden]];
    if(potential_outer_class_info_index != 0) {
        cp_ref outer_class_info_index @ addressof(potential_outer_class_info_index);
    }
    u16 potential_inner_name_index [[hidden]];
    if(potential_inner_name_index != 0) {
        cp_ref inner_name_index @ addressof(potential_inner_name_index);
    }
    u2 inner_class_access_flags;
};

struct attribute_inner_classes {
    u2 number_of_classes;
    inner_class classes[number_of_classes];
};

struct attribute_enclosing_method {
    cp_ref class_index;
    cp_ref method_index;
} [[static]];

struct attribute_synthetic {
};

struct attribute_signature {
    cp_ref signature_index;
} [[static]];

struct attribute_source_file {
    cp_ref sourcefile_index;
} [[static]];

struct attribute_source_debug_extension {
    u1 debug_extension[attribute_length];
};

struct line_number_table_entry {
    u2 start_pc;
    u2 line_number;
} [[static]];

struct attribute_line_number_table {
    u2 line_number_table_length;
    line_number_table_entry line_number_table[line_number_table_length];
};

struct local_variable_table_entry {
    u2 start_pc;
    u2 length;
    cp_ref name_index;
    cp_ref descriptor_index;
    u2 index;
} [[static]];

struct attribute_local_variable_table {
    u2 local_variable_table_length;
    local_variable_table_entry local_variable_table[local_variable_table_length];
};

struct local_variable_type_table_entry {
    u2 start_pc;
    u2 length;
    cp_ref name_index;
    cp_ref signature_index;
    u2 index;
} [[static]];

struct attribute_local_variable_type_table {
    u2 local_variable_type_table_length;
    local_variable_type_table_entry local_variable_type_table[local_variable_type_table_length];
};

struct attribute_deprecated {
};

using annotation;
using element_value;

struct element_value {
    char tag;
    match(tag) {
        ('B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's'): {
            cp_ref const_value_index;
        }
        ('e'): {
            cp_ref type_name_index;
            cp_ref const_name_index;
        }
        ('c'): {
            cp_ref class_info_index;
        }
        ('@'): {
            annotation annotation_value;
        }
        ('['): {
            u2 num_values;
            element_value values[num_values];
        }
        (_): {
            std::error("Unknown element_value tag: %c", tag);
        }
    }
} [[format("fmt::element_value")]];

struct element_value_pair {
    cp_ref element_name_index;
    element_value value;
};

struct annotation {
    cp_ref type_index;
    u2 num_element_value_pairs;
    element_value_pair element_value_pairs[num_element_value_pairs];
};

struct annotation_base {
    u2 num_annotations;
    annotation annotations[num_annotations];
};

struct attribute_runtime_visible_annotations {
    annotation_base [[inline]];
};

struct attribute_runtime_invisible_annotations {
    annotation_base [[inline]];
};

struct attribute_runtime_visible_parameter_annotations {
    u1 num_parameters;
    annotation_base parameter_annotations[num_parameters];
};

struct attribute_runtime_invisible_parameter_annotations {
    u1 num_parameters;
    annotation_base parameter_annotations[num_parameters];
};

struct path_element {
    u1 type_path_kind;
    u1 type_argument_index;
};

struct type_path {
    u1 path_length;
    path_element path[path_length];
};

struct localvar_target_entry {
    u2 start_pc;
    u2 length;
    u2 index;
};

struct target_info<auto target_type> {
    match(target_type) {
        (0x00 | 0x01): { /* type_parameter_target */
            u1 type_parameter_index;
        }
        (0x10): { /* supertype_target */
            u2 supertype_index;
        }
        (0x11 | 0x12): { /* type_parameter_bound_target */
            u1 type_parameter_index;
            u1 bound_index;
        }
        (0x13 | 0x14 | 0x15): { /* empty_target */ }
        (0x16): { /* formal_parameter_target */
            u1 formal_parameter_index;
        }
        (0x17): { /* throws_target */
            u2 throws_type_index;
        }
        (0x40 | 0x41): { /* localvar_target */
            u2 table_length;
            localvar_target_entry table[table_length];
        }
        (0x42): { /* catch_target */
            u2 exception_table_index;
        }
        (0x43 | 0x44 | 0x45 | 0x46): { /* offset_target */
            u2 offset;
        }
        (0x47 | 0x48 | 0x49 | 0x4A): { /* type_argument_target */
            u2 offset;
            u1 type_argument_index;
        }
    }
};

struct type_annotation {
    u1 target_type;
    target_info<target_type> target_info;
    type_path type_path;
    annotation annotation [[inline]];
};

struct attribute_runtime_visible_type_annotations {
    u2 num_annotations;
    type_annotation annotations[num_annotations];
};

struct attribute_runtime_invisible_type_annotations {
    u2 num_annotations;
    type_annotation annotations[num_annotations];
};

struct attribute_annotation_default {
    element_value default_value;
};

struct bootstrap_method {
    cp_ref bootstrap_method_ref;
    u2 num_bootstrap_arguments;
    cp_ref bootstrap_arguments[num_bootstrap_arguments];
};

struct attribute_bootstrap_methods {
    u2 num_bootstrap_methods;
    bootstrap_method bootstrap_methods[num_bootstrap_methods];
};

struct method_parameter {
    u2 potential_name_index [[hidden]];
    if (potential_name_index != 0) {
        cp_ref name_index @ addressof(potential_name_index);
    }
    u2 access_flags;
};

struct attribute_method_parameters {
    u1 parameters_count;
    method_parameter parameters[parameters_count];
};

struct require {
    cp_ref require_index;
    access_flags_module require_flags;
    u2 potential_require_version_index [[hidden]];
    if (potential_require_version_index != 0) {
        cp_ref require_version_index @ addressof(potential_require_version_index);
    }
};

struct export {
    cp_ref exports_index;
    access_flags_module exports_flags;
    u2 exports_to_count;
    cp_ref exports_to_index[exports_to_count];
};

struct open {
    cp_ref opens_index;
    access_flags_module opens_flags;
    u2 opens_to_count;
    cp_ref opens_to_index[opens_to_count];
};

struct provide {
    cp_ref provides_index;
    u2 provides_with_count;
    cp_ref provides_with_index[provides_with_count];
};

struct attribute_module {
    cp_ref module_name_index;
    u2 module_flags;
    u2 potential_module_version_index [[hidden]];
    if (potential_module_version_index != 0) {
        cp_ref module_version_index @ addressof(potential_module_version_index);
    }
    u2 requires_count;
    require requires[requires_count];
    u2 exports_count;
    export exports[exports_count];
    u2 opens_count;
    open opens[opens_count];
    u2 uses_count;
    cp_ref uses_index[uses_count];
    u2 provides_count;
    provide provides[provides_count];
};

struct attribute_module_packages {
    u2 package_count;
    cp_ref package_index[package_count];
};

struct attribute_module_main_class {
    cp_ref main_class_index;
};

struct attribute_nest_host {
    cp_ref host_class_index;
};

struct attribute_nest_members {
    u2 number_of_classes;
    cp_ref classes[number_of_classes];
};

using record_component_info;

struct attribute_record {
    u2 component_count;
    record_component_info components[component_count];
};

struct attribute_permitted_subclasses {
    u2 number_of_classes;
    cp_ref classes[number_of_classes];
};

struct attribute_info<auto constant_pool> {
    cp_ref attribute_name_index;
    u4 attribute_length;
    match(constant_pool[attribute_name_index-1].bytes) {
        ("Code"): attribute_code child [[inline]];
        ("ConstantValue"): attribute_constant_value [[inline]];
        ("StackMapTable"): attribute_stack_map_table [[inline]];
        ("Exceptions"): attribute_exception [[inline]];
        ("InnerClasses"): attribute_inner_classes [[inline]];
        ("EnclosingMethod"): attribute_enclosing_method [[inline]];
        ("Synthetic"): attribute_synthetic [[inline]];
        ("Signature"): attribute_signature [[inline]];
        ("SourceFile"): attribute_source_file [[inline]];
        ("SourceDebugExtension"): attribute_source_debug_extension [[inline]];
        ("LineNumberTable"): attribute_line_number_table [[inline]];
        ("LocalVariableTable"): attribute_local_variable_table [[inline]];
        ("LocalVariableTypeTable"): attribute_local_variable_type_table [[inline]];
        ("Deprecated"): attribute_deprecated [[inline]];
        ("RuntimeVisibleAnnotations"): attribute_runtime_visible_annotations [[inline]];
        ("RuntimeInvisibleAnnotations"): attribute_runtime_invisible_annotations [[inline]];
        ("RuntimeVisibleParameterAnnotations"): attribute_runtime_visible_parameter_annotations [[inline]];
        ("RuntimeInvisibleParameterAnnotations"): attribute_runtime_invisible_parameter_annotations [[inline]];
        ("RuntimeVisibleTypeAnnotations"): attribute_runtime_visible_type_annotations [[inline]];
        ("RuntimeInvisibleTypeAnnotations"): attribute_runtime_invisible_type_annotations [[inline]];
        ("AnnotationDefault"): attribute_annotation_default [[inline]];
        ("BootstrapMethods"): attribute_bootstrap_methods [[inline]];
        ("MethodParameters"): attribute_method_parameters [[inline]];
        ("Module"): attribute_module [[inline]];
        ("ModulePackages"): attribute_module_packages [[inline]];
        ("ModuleMainClass"): attribute_module_main_class [[inline]];
        ("NestHost"): attribute_nest_host [[inline]];
        ("NestMembers"): attribute_nest_members [[inline]];
        ("Record"): attribute_record [[inline]];
        (_): u1 info[attribute_length];
    }
    if(($-(addressof(this)+6)) != attribute_length) std::warning("Invalid attribute length at "
     + std::format("{:x}", addressof(this))
     + " " + std::string::to_string($-(addressof(this)+6)) + "!=" + std::string::to_string(attribute_length));
} [[format("fmt::attribute")]];

struct record_component_info {
    cp_ref name_index;
    cp_ref descriptor_index;
    u2 attributes_count;
    attribute_info<parent.parent.parent.constant_pool> attributes[attributes_count];
};

struct exception {
    u2 start_pc;
    u2 end_pc;
    u2 handler_pc;
    u2 catch_type;
} [[static]];

struct match_offset {
    u4 match_case;
    u4 offset;
};

struct instruction {
    u1 mnemonic [[format("fmt::byte_code_fmt")]];
    match(mnemonic) {
        (0 ... 15 | 26 ... 53 | 59 ... 131 | 133 ... 152 | 172 ... 177 | 190 | 191 | 194 | 195): {}
        (16): {
            u1 byte;
        }
        (17): {
            u2 value;
        }
        (18):{
            u1 index [[format("fmt::const_ref_top")]];
        }
        (19|20 | 178 ... 184 | 187 | 189 | 192 | 193): {
            cp_ref cp_index;
        }
        (21 ... 25 | 54 ... 58 | 169): {
            u1 local_index;
        }
        (132): {
            u1 local_index;
            u1 const_;
        }
        (153 ... 168 | 198 | 199): {
            u2 branch_offset;
        }
        (185): {
            cp_ref index;
            u1 count;
            padding[1];
        }
        (186): {
            cp_ref cp_index;
            padding[2];
        }
        (188): {
            u1 atype [[format("fmt::atype_fmt")]];
        }
        (196): {
            u1 op_code [[format("fmt::byte_code_fmt")]];
            u2 local_index;
            if(op_code == 132){
                u2 const_byte;
            }
        }
        (197): {
            cp_ref cp_index;
            u1 dimensions;
        }
        (200 | 201): {
            u4 branch_offset;
        }
        (170): {
            padding[(4 - ($ - addressof(parent) -8) % 4) % 4];
            u4 default_offset;
            u4 low;
            u4 high;
            u4 jump_offset[high-low+1];
        }
        (171): {
            padding[(4 - ($ - addressof(parent) -8) % 4) % 4];
            u4 default_offset;
            u4 npairs;
            match_offset match_offsets[npairs];
        }
    }
} [[format("fmt::instruction_code_fmt")]];


struct attribute_code {
    u2 max_stack;
    u2 max_locals;
    u4 code_length;
    u4 target_addr = $ + code_length;
    instruction code[while($ < target_addr)];
    u2 exception_table_length;
    exception exception_table[exception_table_length];
    u2 attributes_count;
    attribute_info<parent.parent.parent.constant_pool> attributes[attributes_count];
};


struct field_info {
    access_flags_field access_flags;
    cp_ref name_index;
    cp_ref descriptor_index;
    u2 attribute_count;
    attribute_info<parent.constant_pool> attributes[attribute_count];
};

struct method_info {
    access_flags_method access_flags;
    cp_ref name_index;
    cp_ref descriptor_index;
    u2 attribute_count;
    attribute_info<parent.constant_pool> attributes[attribute_count];
};

struct ClassFile {
    u1 magic[4];
    u2 minor_version;
    major_version major_version;
    u2 constant_pool_count;
    cp_info constant_pool[constant_pool_count-1];
    access_flags_class access_flags;
    cp_ref this_class;
    cp_ref super_class;
    u2 interfaces_count;
    cp_ref interfaces[interfaces_count];
    u2 fields_count;
    field_info fields[fields_count];
    u2 methods_count;
    method_info methods[methods_count];
    u2 attributes_count;
    attribute_info<constant_pool> attributes[attributes_count];
};

ClassFile file @ 0x0;
