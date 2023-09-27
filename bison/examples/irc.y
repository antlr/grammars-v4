// This code from https://begriffs.com/posts/2021-11-28-practical-parsing.html
/* irc.y  (Bison only)

   Using Bison mostly for the %code positions, making 
   it easier to use libderp between flex and bison.

   - WARNING -
   There is absolutely no memory hygiene in this example.
   We don't check for allocation failure, and we don't free
   things when done. See the earlier lisp.y/.l examples
   for guidance about that.
*/

/* output more descriptive messages than "syntax error" */
%define parse.error verbose

%code top {
	#define _XOPEN_SOURCE 600
	#include <stdio.h>
	#include <stdlib.h>
}

%code requires {
	#include <derp/list.h>
	#include <derp/treemap.h>

	struct prefix
	{
		char *host;
		char *nick;
		char *user;
	};

	/* building an irc_message is the overall
	   goal for this parser */
	struct irc_message
	{
		treemap *tags;
		struct prefix *prefix;
		char *command;
		list *params;
	};
}

%code provides {
	int yyerror(char const *msg);
	int yylex(void);
	void message_print(struct irc_message *m);
}

%union
{
	char *str;
	struct prefix *prefix;
	treemap *map;
	struct map_pair *pair;
	list *list;
	struct irc_message *msg;
}

%token          SPACE
%token <str>    COMMAND MIDDLE TRAILING
%token <pair>   TAG
%token <prefix> PREFIX

%type <msg> message tagged_message prefixed_message
%type <map> tags
%type <list> params

%%

 /* Like in the CSV example, we start with a dummy
    rule just to add side-effects */

final :
  tagged_message { message_print($1); }
;

 /* Messages begin with two optional components,
    a set of tags and a prefix.
 
    <message> ::= ['@' <tags> <SPACE>] [':' <prefix> <SPACE> ] <command> [params]
 
    Rather than making a single message rule with
    tons of variations (and duplicated code), I chose
    to build the message in stages.
 
    tagged_message <- prefixed_message <- message
 
    A prefixed_message adds prefix information, or
    passes the message along verbatim if there is none.
    Similarly for tagged_message. */

tagged_message :

  /* When there are more than one matched token,
     it's helpful to add Bison "named references"
     in brackets. Thus, below, the rule can refer to
     $ts rather than $2, or $msg rather than $4.
     Makes it way easier to rearrange tokens while
     you're experimenting. */

  '@' tags[ts] SPACE prefixed_message[msg] {
	$msg->tags = $ts;
	$$ = $msg;
  }

  /* here's the pass-through case when there are
     no tags on the message */

| prefixed_message
;

prefixed_message :
  ':' PREFIX[pfx] SPACE message[msg] {
	$msg->prefix = $pfx;
	$$ = $msg;
  }
| message
;

message :
  COMMAND[cmd] params[ps] {
	struct irc_message *m = malloc(sizeof *m);
	*m = (struct irc_message) {
		.command=$cmd, .params=$ps
	};
	$$ = m;
  }
;

tags :
  TAG {
	treemap *t = tm_new(derp_strcmp, NULL);
	tm_insert(t, $1->k, $1->v);
	$$ = t;
  }
| tags[ts] ';' TAG[t] {
	tm_insert($ts, $t->k, $t->v);
	$$ = $ts;
  }
;

params :
  SPACE TRAILING {
	$$ = l_new();
	l_prepend($$, $2);
  }
| SPACE MIDDLE[mid] params[ps] {
	l_prepend($ps, $mid);
	$$ = $ps;
  }
| %empty {
	$$ = l_new();
  }
;

%%

int yyerror(char const *msg)
{
	return fprintf(stderr, "%s\n", msg);
}

void message_print(struct irc_message *m)
{
	if (m->tags)
	{
		struct tm_iter  *it = tm_iter_begin(m->tags);
		struct map_pair *p;

		puts("Tags:");
		while ((p = tm_iter_next(it)) != NULL)
			printf("\t'%s'='%s'\n", (char*)p->k, (char*)p->v);
		tm_iter_free(it);
	}
	if (m->prefix)
		printf("Prefix: Nick %s, User %s, Host %s\n",
		       m->prefix->nick, m->prefix->user,
			   m->prefix->host);
	if (m->command)
		printf("Command: %s\n", m->command);
	if (!l_is_empty(m->params))
	{
		puts("Params:");
		for (list_item *li = l_first(m->params); li; li = li->next)
			printf("\t%s\n", (char*)li->data);
	}
}