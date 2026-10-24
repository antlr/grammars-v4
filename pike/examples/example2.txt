int main(int argc, array (string) argv)
{
    if(argc <= 1)
    {
        list_records();
    } else {
        show_record((int) argv[1]);
    }
}
