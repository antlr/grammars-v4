/* Test using -> to iterate through a linked list
 * and exercise chained member access of the form x->y->
 * */
void *malloc(unsigned long size);

// linked_list_node type from Listing 18-6
struct linked_list_node {
    int val;
    struct linked_list_node *next;
};

struct linked_list_node *array_to_list(int *array, int count) {
    struct linked_list_node *head =
        (struct linked_list_node *)malloc(sizeof(struct linked_list_node));
    head->val = array[0];
    head->next = 0;
    struct linked_list_node *current = head;
    for (int i = 1; i < count; i = i + 1) {
        current->next =
            (struct linked_list_node *)malloc(sizeof(struct linked_list_node));
        current->next->next = 0;
        current->next->val = array[i];
        current = current->next;
    }
    return head;
}

int main(void) {
    int arr[4] = {9, 8, 7, 6};
    struct linked_list_node *elem = array_to_list(arr, 4);

    for (int i = 0; i < 4; i = i + 1) {
        int expected = arr[i];
        if (elem->val != expected) {
            return i + 1;  // return 1 if 0th element is wrong, 2 if 1st elem is
                           // wrong, etc.
        }
        elem = elem->next;
    }
    return 0;  // success
}