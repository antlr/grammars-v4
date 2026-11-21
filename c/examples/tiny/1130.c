static unsigned long diff_filespec_size(struct diff_filespec *one){

	if (!DIFF_FILE_VALID(one))

		return 0;

	diff_populate_filespec(one, CHECK_SIZE_ONLY);

	return one->size;
}
