// any statement can have a label

int main(void) {
    int a = 1;
label_if:
    if (a)
        goto label_expression;
    else
        goto label_empty;

label_goto:
    goto label_return;

    if (0)
    label_expression:
        a = 0;

    goto label_if;

label_return:
    return a;

label_empty:;
    a = 100;
    goto label_goto;
}