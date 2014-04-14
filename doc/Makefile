
doc_dir = html
all_docs = $(doc_dir)/*.html $(doc_dir)/*.png \
		$(doc_dir)/*.css $(doc_dir)/edoc-info
src_dir = ../src
src_files = $(wildcard $(src_dir)/*.erl)
	
# Create an input line suitable for erl specifying a list of files
# to run through edoc. The result should look something like:
#     '["../src/happy.erl", "../src/grumpy.erl"]'
comma := ,
empty := 
space := $(empty) $(empty)
edoc_files = $(src_files:$(src_dir)/%.erl="$(src_dir)/%.erl")
edoc_files := $(strip $(edoc_files))
edoc_files := [$(subst $(space),$(comma) ,$(edoc_files))]

tar_dir = $(APP_NAME)-doc-$(VER)
tgz_name = $(tar_dir).tar.gz

all: app_overview mod_docs 

app_overview: 
	erl -noshell -run edoc_run application \
		"'$(APP_NAME)'" \
		'"$(doc_dir)"' \
		'[{dir, "$(doc_dir)"}, {def, {version, "$(VER)"}}]'

mod_docs: 
	erl -noshell -run edoc_run files \
		'$(edoc_files)' \
		'[{dir, "$(doc_dir)"}, {def, {version, "$(VER)"}}]'

dist_doc: all
	cp -a $(doc_dir) $(tar_dir) && \
		tar zcvf ../$(tgz_name) $(tar_dir) && \
		rm -fr $(tar_dir)

clean:
	rm -f $(all_docs)

