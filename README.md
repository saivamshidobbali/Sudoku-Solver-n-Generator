# Implemented Sudoku Solver and Generator in Racket.

Solver:  
  
3x3 board  
#(5 3 0 0 7 0 0 0 0)  
#(6 0 0 1 9 5 0 0 0)  
#(0 9 8 0 0 0 0 6 0)  
#(8 0 0 0 6 0 0 0 3)  
#(4 0 0 8 0 3 0 0 1)  
#(7 0 0 0 2 0 0 0 6)  
#(0 6 0 0 0 0 2 8 0)  
#(0 0 0 4 1 9 0 0 5)  
#(0 0 0 0 8 0 0 7 9)  
  
Solved 3x3 Board  
#(5 3 4 6 7 8 9 1 2)    
#(6 7 2 1 9 5 3 4 8)  
#(1 9 8 3 4 2 5 6 7)  
#(8 5 9 7 6 1 4 2 3)  
#(4 2 6 8 5 3 7 9 1)  
#(7 1 3 9 2 4 8 5 6)  
#(9 6 1 5 3 7 2 8 4)  
#(2 8 7 4 1 9 6 3 5)  
#(3 4 5 2 8 6 1 7 9)  
  
3x2 board  
#(6 0 3 0 4 1)  
#(0 0 0 0 0 5)  
#(0 0 0 0 2 0)  
#(0 4 0 0 0 0)  
#(5 0 0 0 0 0)  
#(3 1 0 5 0 4)  
  
Solved 3x2 Board  
#(6 5 3 2 4 1)   
#(4 2 1 6 3 5)  
#(1 3 5 4 2 6)  
#(2 4 6 1 5 3)  
#(5 6 4 3 1 2)  
#(3 1 2 5 6 4)  
  
4x4 board  
#(0 2 3 0 0 6 0 0 9 0 11 0 0 0 0 0)  
#(5 6 0 8 1 0 3 0 0 14 0 0 0 10 0 12)  
#(0 10 11 12 13 14 0 16 1 0 0 4 5 6 0 8)  
#(0 14 0 16 0 0 11 0 5 6 7 0 0 0 0 4)  
#(2 1 0 3 6 0 8 7 10 0 12 11 14 0 16 0)  
#(6 5 8 0 2 0 4 3 14 0 16 0 10 9 12 11)  
#(0 9 12 0 0 13 0 15 2 0 4 3 6 0 8 7)  
#(0 0 0 15 10 0 0 11 6 5 0 7 0 0 4 3)  
#(0 4 0 0 0 0 5 0 0 0 0 10 15 0 13 0)  
#(0 8 0 6 3 4 1 2 0 0 13 14 0 0 9 10)  
#(0 12 9 0 0 0 13 0 0 0 0 2 0 0 5 0)  
#(15 0 13 14 11 12 9 0 0 8 0 0 0 4 0 0)  
#(4 0 0 0 0 7 6 5 0 11 0 0 16 15 14 13)  
#(8 7 6 0 0 0 2 1 16 0 0 0 0 0 10 0)  
#(0 0 10 0 16 15 14 0 4 3 0 0 0 0 6 0)  
#(16 15 0 0 0 0 10 9 8 7 6 5 0 0 0 1)  
   
Solved 4x4 Board  
#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)  
#(5 6 7 8 1 2 3 4 13 14 15 16 9 10 11 12)  
#(9 10 11 12 13 14 15 16 1 2 3 4 5 6 7 8)  
#(13 14 15 16 9 10 11 12 5 6 7 8 1 2 3 4)  
#(2 1 4 3 6 5 8 7 10 9 12 11 14 13 16 15)  
#(6 5 8 7 2 1 4 3 14 13 16 15 10 9 12 11)  
#(10 9 12 11 14 13 16 15 2 1 4 3 6 5 8 7)  
#(14 13 16 15 10 9 12 11 6 5 8 7 2 1 4 3)  
#(3 4 1 2 7 8 5 6 11 12 9 10 15 16 13 14)  
#(7 8 5 6 3 4 1 2 15 16 13 14 11 12 9 10)  
#(11 12 9 10 15 16 13 14 3 4 1 2 7 8 5 6)  
#(15 16 13 14 11 12 9 10 7 8 5 6 3 4 1 2)  
#(4 3 2 1 8 7 6 5 12 11 10 9 16 15 14 13)  
#(8 7 6 5 4 3 2 1 16 15 14 13 12 11 10 9)  
#(12 11 10 9 16 15 14 13 4 3 2 1 8 7 6 5)  
#(16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)  

Generator:

Generated 3x3 Board
#(1 0 3 0 5 0 0 0 0)  
#(0 0 6 0 0 9 1 2 0)  
#(7 0 9 0 0 0 0 5 0)  
#(0 0 4 0 0 5 0 0 7)  
#(3 0 5 0 9 7 2 0 0)  
#(8 9 7 2 1 4 0 6 5)  
#(0 0 0 0 0 0 0 7 0)  
#(0 4 0 0 0 0 0 0 1)  
#(0 0 0 5 3 1 6 0 0)  

Generated 4x4 Board  
#(0 0 0 4 5 0 7 8 0 0 0 12 0 14 0 16)  
#(5 0 0 8 0 2 0 0 0 0 0 16 0 0 0 12)    
#(9 10 11 0 0 0 15 0 1 0 0 4 0 0 0 8)  
#(0 0 15 0 9 0 0 0 5 0 0 0 1 2 0 4)  
#(2 0 4 0 6 0 0 7 10 9 12 11 0 13 16 0)  
#(6 5 8 7 2 1 4 0 14 0 16 0 10 0 12 11)  
#(0 9 12 11 0 13 0 0 0 1 4 0 6 0 8 7)  
#(14 13 0 0 10 0 0 0 6 0 0 7 0 1 4 0)  
#(3 0 0 0 7 0 0 0 11 0 0 10 15 16 13 0)  
#(0 8 5 0 0 4 1 2 15 0 13 14 0 12 0 10)  
#(11 0 9 10 0 0 13 14 0 4 1 0 7 8 5 6)  
#(15 0 13 14 11 0 0 10 7 0 0 0 0 4 1 2)   
#(4 0 0 1 0 0 6 5 12 11 10 9 16 0 0 0)  
#(8 7 6 5 0 0 0 1 0 15 0 0 0 0 0 9)  
#(0 11 0 0 16 0 0 0 4 0 0 1 8 7 6 5)  
#(16 15 14 0 0 11 10 0 8 0 6 0 0 0 0 0)  
