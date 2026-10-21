#pragma description Dalvik EXecutable
#pragma magic [ 64 65 78 0A ?? ?? ?? 00 ]

import type.leb128;

struct header_item {
    u8 magic[8];
    u32 checksum;
    u8 signature[20];
    u32 file_size;
    u32 header_size;
    u32 endian_tag;
    u32 link_size;
    u32 link_off;
    u32 map_off;
    u32 string_ids_size;
    u32 string_ids_off;
    u32 type_ids_size;
    u32 type_ids_off;
    u32 proto_ids_size;
    u32 proto_ids_off;
    u32 field_ids_size;
    u32 field_ids_off;
    u32 method_ids_size;
    u32 method_ids_off;
    u32 class_defs_size;
    u32 class_defs_off;
    u32 data_size;
    u32 data_off;
};

struct map_item {
    u16 type;
    u16 unused;
    u32 size;
    u32 offset;
};

struct map_list {
    u32 size;
    map_item list[size];
};

struct string_data_item {
    type::uLEB128 utf16_size[[hidden]];
    char string[utf16_size];
}[[inline]];

struct string_id_item {
    string_data_item* string_data: u32;
}[[name(string_data.string)]];


struct type_id_item {
    u32 descriptor_idx;
    char type_name[] @ addressof(parent.string_ids[descriptor_idx].string_data.string);
}[[name(type_name)]];

struct proto_id_item {
    u32 shorty_idx;
    u32 return_type_idx;
    u32 parameters_off;
    char shorty_dec[] @ addressof(parent.string_ids[shorty_idx].string_data.string);
    char return_type[] @ addressof(parent.type_ids[return_type_idx].type_name);
}[[name(shorty_dec)]];

struct field_id_item {
    u16 class_idx;
    u16 type_idx;
    u32 name_idx;
    char class_name[] @ addressof(parent.type_ids[class_idx].type_name);
    char type_name[] @ addressof(parent.type_ids[type_idx].type_name);
    char field_name[] @ addressof(parent.string_ids[name_idx].string_data.string);
}[[name(field_name)]];

struct method_id_item {
    u16 class_idx;
    u16 proto_idx;
    u32 name_idx;
    char class_name[] @ addressof(parent.type_ids[class_idx].type_name);
    char proto_desc[] @ addressof(parent.proto_ids[proto_idx].shorty_dec);
    char method_name[] @ addressof(parent.string_ids[name_idx].string_data.string);

}[[name(class_name+method_name)]];

struct class_site_id_item {
    u32 call_site_off;
};

struct method_handle_item {
    u16 method_handle_type;
    u16 unused;
    u16 field_or_method_id;
    u16 unused2;
};
enum access_flag : type::uLEB128{
    public = 0x1,
    private = 0x2,
    protected = 0x4,
    static = 0x8,
    final = 0x10,
    synchronized = 0x20,
    volatile = 0x40
};

struct encoded_field {
    type::uLEB128 field_idx_diff;
    access_flag access_flags;
};

struct encoded_method {
    type::uLEB128 method_idx_diff;
    type::uLEB128 access_flags;
    type::uLEB128 code_off;
};

struct class_data_item {
    type::uLEB128 static_fields_size;
    type::uLEB128 instance_fields_size;
    type::uLEB128 direct_methods_size;
    type::uLEB128 virtual_methods_size;
    encoded_field static_fields[static_fields_size];
    encoded_field instance_fields[instance_fields_size];
    encoded_method direct_methods[direct_methods_size];
    encoded_method virtual_methods[virtual_methods_size];
};

struct class_def_item {
    u32 class_idx;
    u32 access_flags;
    u32 superclass_idx;
    u32 interfaces_off;
    u32 source_file_idx;
    u32 annotations_off;
    u32 class_data_off;
    //class_data_item *class_data_off:u32;
    u32 static_values_off;
    char class_name[] @ addressof(parent.type_ids[class_idx].type_name);
}[[name(class_name)]];

struct type_item {
    u16 type_idx;
};

struct type_list {
    u32 size;
    type_item list[size];
};

struct code_item {
    u16 registers_size;
    u16 ins_size;
    u16 outs_size;
    u16 tries_size;
    u32 debug_info_off;
    u32 insns_size;
    u16 insns[insns_size];
};

struct try_item {
    u32 start_addr;
    u16 insn_count;
    u16 handler_off;
};



struct Dex {
    header_item header;
    string_id_item string_ids[header.string_ids_size] @ header.string_ids_off;
    type_id_item type_ids[header.type_ids_size] @ header.type_ids_off;
    proto_id_item proto_ids[header.proto_ids_size] @ header.proto_ids_off;
    field_id_item field_ids[header.field_ids_size] @ header.field_ids_off;
    method_id_item method_ids[header.method_ids_size] @ header.method_ids_off;
    class_def_item class_defs[header.class_defs_size] @ header.class_defs_off;
    u8 data[header.data_size] @header.data_off;
    map_list map_list @ header.map_off;
    u8 link_data[header.link_size] @ header.link_off;
};
Dex dex @ 0x00;
