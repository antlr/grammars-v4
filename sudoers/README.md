# sudoers Grammar

An ANTLR4 grammar for the [sudoers](https://www.sudo.ws/docs/man/sudoers.man/) file format,
the configuration file used by [sudo](https://www.sudo.ws/) to control which users may run
which commands as which other users on a host.

## References

* [sudoers(5) man page](https://www.sudo.ws/docs/man/sudoers.man/) — the authoritative
  description of the file format.
* [BNF diagram for sudoers file](https://serverfault.com/questions/696085/bnf-diagram-for-sudo-file)
  — a community-authored grammar reference that this grammar is based on.
