#! /usr/bin/lib/python3
# -*- coding:utf-8 -*-

def parentheses(s):
    ouv = "([{<"
    fer = ")]}>"
    p = []
    for i,ch in enumerate(s):
        if ch in ouv:
            p.append((ch,i))
        elif ch in fer:
            if p != []:
                cho, io = p.pop()
                if ouv.index(cho)==fer.index(ch):
                    print(io,i)
                else:
                    return False
            else:
                return False
    return p == []

op = {"+":((lambda x,y:y+x),2),
      "*":((lambda x,y:x*y),2),
      "-":((lambda x:-x),1),
      "/":((lambda x:1/x),1),
      "incr":((lambda x:x+1),1),
      "sq":((lambda x:x*x),1)}

def eval_npi(exp):
    p = []
    for e in exp:
        if isinstance(e,str):
            fun,dom = op[e]
            p.append(fun(*[p.pop() for i in range(dom)]))
        else:
            p.append(e)
    return p.pop()
