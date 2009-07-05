.SUFFIXES: .erl .beam

.erl.beam:
	
	erlc -v -W -o ../ebin $<

srcdir=$(cd/src)


MODS=$(srcdir) vertex_server  graph_server dijk
              
		

ERL=erl -boot -pa  start_clean

all: compile
	

compile:${MODS:%=%.beam} 

clean:
	rm -rf *.beam erl_crash.dump

