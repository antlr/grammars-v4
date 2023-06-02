// https://github.com/antlr/grammars-v4/issues/3484

import { "a-b" as a } from "/modules/my-module.js";
import { "a-b" as yield } from "/modules/my-module.js";
import { "a-b" as await } from "/modules/my-module.js";

const a = 1;
export { a as "a-b" };