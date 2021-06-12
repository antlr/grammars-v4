operator>() {}

A<B<C>> not_right_shift;

auto zero_parameter_lambda_with_assign = [=] () -> A <B> {};

std::string rawstring_one_line = R"a(b)c";

extern "C" {
	std::string literal_newline = "new\
	line";
}
