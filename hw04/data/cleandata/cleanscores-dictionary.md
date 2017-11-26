
## Data `rawscores.csv`

The data contains the raw scores for fictitious Stat 133 class. 
There are 335 rows and 23 columns.

The column labels are listed below with their appropriate units:
- homework assignments: columns HW1 to HW9, each 100 pts
- lab attendance: ATT, number of attended labs (0 to 12)
- quiz scores:
    -QZ1, 12 pts
    -QZ2, 18 pts
    -QZ3, 20 pts
    -QZ4, 20 pts
- exam1: EX1, 80pts
- exam2: EX2, 90pts
- Test1: exam1 in percentage out of 100
- Test2: exam2 in percentage out of 10
- Lab: lab scoreout of 12
- Homework: homework score using score_homework function
- Quiz: homework score using score_homework quiz
- Overall: 0.1*Lab + 0.3*Homework + 0.15*Quiz + 0.2*Test1 + 0.25*Test2
- Grade: Letter grade
