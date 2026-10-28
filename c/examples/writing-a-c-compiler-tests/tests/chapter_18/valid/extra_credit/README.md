Valid extra credit tests are organized into subdirectories as follows:
* `libraries/`: Library/client pairs to test that handling of unions is ABI-compliant (including memory layout, initialization, and calling conventions)
* `member_access/`: Test cases for accessing union members with `.` and `->` operators
* `other_features/`: Tests of interaction between structures and extra-credit features other than union
* `semantic_analysis/`: Test cases focused on identifier resolution and type checking. We're more concerned with validating that the semantic analysis stage accepts these than we are with validating their behavior.
* `size_and_offset/`: Tests validating that we calculate union sizes and member offsets correctly.
* `union_copy/`: copying entire unions from one location to another (including simple assignment and copying through pointers)