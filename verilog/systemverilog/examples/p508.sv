typedef enum bit [1:0] {
  A = 2'b00,
  B = 2'b11
} ab_e;
typedef struct packed {ab_e ValidAB;} VStructEnum;
typedef union packed {ab_e ValidAB;} VUnion;
