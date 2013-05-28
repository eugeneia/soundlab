.PHONY : all
all : example.lisp demo-els13

example.lisp :
	m4 -Itutorial/ tutorial/examples.lisp.m4 > example.lisp

.PHONY : demo-els13
demo-els13 : demo-els13/demo.mk10 \
             demo-els13/slides.mk10

demo-els13/demo.mk10 :
	m4 -Idemo-els13/ \
        demo-els13/demo.mk10.m4 > demo-els13/demo.mk10

demo-els13/slides.mk10 :
	m4 -Idemo-els13/ -Dslides \
        demo-els13/demo.mk10.m4 > demo-els13/slides.mk10

.PHONY : clean
clean :
	rm -f example.lisp demo-els13/*.mk10
