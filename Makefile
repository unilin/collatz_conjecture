PROJECT = collatz_conjecture
# --------------------------------------------------------------------
# Defining OTP version for this project which uses by kerl
# --------------------------------------------------------------------
ERLANG_OTP = OTP-21.0

# global compile options (about more warnings see erl_lint.erl)
ERLC_GLOBAL_OPTS = +'{parse_transform, lager_transform}'

# default mode
ERLC_OPTS = $(ERLC_GLOBAL_OPTS)
#ERLC_OPTS += +warn_export_all +warn_export_vars +warn_unused_import +warn_untyped_record +warn_missing_spec_all
ERLC_OPTS += -Werror
ERLC_OPTS += +debug_info

# tests mode
TEST_ERLC_OPTS += $(ERLC_GLOBAL_OPTS)
TEST_ERLC_OPTS += +debug_info

# --------------------------------------------------------------------
#  # Dependencies.
# --------------------------------------------------------------------

# our
dep_teaser = git https://github.com/spylik/teaser					master
dep_sync = git https://github.com/rustyio/sync						master


DEPS =teaser lager

SHELL_DEPS = sync

# --------------------------------------------------------------------
# Development enviroment ("make shell" to run it).
# --------------------------------------------------------------------

SHELL_OPTS = -kernel shell_history enabled -args_file vm.args.development -config sys.config -pa ebin/ test/ -eval 'lager:start(), mlibs:discover()' -env ERL_LIBS deps -run mlibs autotest_on_compile

# --------------------------------------------------------------------
# We using erlang.mk 
# --------------------------------------------------------------------

SHELL_OPTS = -pa ebin/ test/ -env ERL_LIBS deps -eval 'code:ensure_loaded(piglatin), code:ensure_loaded(piglatin_tests), ok = sync:go(), RunTests = fun(Mods) -> _ = [Mod:test() || Mod <- Mods, erlang:function_exported(Mod, test, 0)], [eunit:test(Mod) || Mod <- Mods] end, sync:onsync(RunTests)'

include erlang.mk
