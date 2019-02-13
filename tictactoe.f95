
program main
!
!   This program plays the game Tic-Tac-Toe with
!   the user and the computer AI. The user is player "X"
!   and the computer AI is player "O". The game continues
!   until the one of the two players win, or if its a draw
!
!
! Call the playTicTacToe subroutine to start playing the game
    call playTicTacToe
end program main

subroutine playTicTacToe
    implicit none
!
! Subroutine which plays the game. Assume one player is human and the other player is a computer.
!
! Local variables & functions used
    character, dimension(3,3) :: tictac
    integer :: userMove, getMove
    logical :: CHKPLAY

! Initialize the squares on the tictactoe board with spaces
    tictac = " "

! Calling subroutine to display the introduction of the game
    call displayIntro

! Loop the game until a winner has been declared, or until the game comes to a draw
    do
        write (*,*) 'Your move?'
        userMove = getMove(userMove)
!
! Check to see if the human player's move is valid using the CHKPLAY subroutine, and continue to the next line if CHKPLAY returns true
        if (CHKPLAY(tictac, userMove)) then
            write(*,*) 'After your move'
! Using the number that the human player has chosen, check the conditions and place the X in the appropriate square
            select case (userMove)
                case(1)
                    tictac(1,1) = "X"
                case(2)
                    tictac(1,2) = "X"
                case(3)
                    tictac(1,3) = "X"
                case(4)
                    tictac(2,1) = "X"
                case(5)
                    tictac(2,2) = "X"
                case(6)
                    tictac(2,3) = "X"
                case(7)
                    tictac(3,1) = "X"
                case(8)
                    tictac(3,2) = "X"
                case(9)
                    tictac(3,3) = "X"
            end select
! Print the board to the screen after the human player X has made a move
            call showBoard(tictac)
! Check for a winner or if game is a draw
            call CHKOVR(tictac)
! Call computer AI subroutine for computer player O to make a move
            call pickMove(tictac)
            write(*,*) 'After my move'
! Print the board to the screen after the computer player O has made a move
            call showBoard(tictac)
! Check for a winner or if game is a draws
            call CHKOVR(tictac)
!
! If the CHKPLAY subroutine returns FALSE, then send feedback to the human saying that their move is invalid
        else
            write(*,*) 'Cannot move there! Please try again!'
        endif
    enddo

end subroutine playTicTacToe

subroutine displayIntro
!
! Prints the introduction of the tictactoe game and basic instructions on how to play
    write (*,*) "PLAY TIC-TAC-TOE. Enter 1-9 TO PLAY"
    write (*,*) "           1 | 2 | 3 "
    write (*,*) "          ---+---+---"
    write (*,*) "           4 | 5 | 6 "
    write (*,*) "          ---+---+---"
    write (*,*) "           7 | 8 | 9 "
end subroutine displayIntro

integer function getMove(userMove)
    implicit none
!
! Gets the humans move. The output from getMove() is a number from 1 to 9 representing
! the chosen square. Valids player input.
!
! Argument Definitions --
!   Input Arguments
!       userMove - represents the number of the square that the human player has chosen to make a play on
    integer :: userMove
!
! Local variables
    integer :: error
    logical :: invalidInput

! Initialize local variable
!   - assume that the human player has entered invalid input
    invalidInput = .TRUE.

! Loop until the human player has entered valid input
    do while(invalidInput)
        read (*,*,iostat=error) userMove
        if ((error == 0) .AND. (0 < userMove) .AND. (10 > userMove)) then
            invalidInput = .FALSE.
        else
            write (*,*) "Invalid input! Enter a number between 1-9"
            write (*,*) 'Your move?'
        endif
    enddo
! Return the value in the local variable to the caller
    getMove = userMove
    return
end function getMove

logical function CHKPLAY(tictac, userMove)
    implicit none
!
! Checks to make sure the human player cannot make a play in a square that is already occupied.
!
! Argument Definitions --
!   Input Arguments
!       tictac - represents the current state of the board game
!       userMove - represents the number of the square that the human player has chosen to make a play on
    character, dimension(3,3) :: tictac
    integer :: userMove

! Assume that the player can make a play in the chosen sqaure
    CHKPLAY = .TRUE.

!
! Using the number that the user has chosen, check the conditions and return TRUE if the square is empty
! or return FALSE if the square is already occupied
    select case (userMove)
        case (1)
            if (tictac(1,1) /= " ") goto 2
            return
        case (2)
            if (tictac(1,2) /= " ") goto 2
            return
        case (3)
            if (tictac(1,3) /= " ") goto 2
            return
        case (4)
            if (tictac(2,1) /= " ") goto 2
            return
        case (5)
            if (tictac(2,2) /= " ") goto 2
            return
        case (6)
            if (tictac(2,3) /= " ") goto 2
            return
        case (7)
            if (tictac(3,1) /= " ") goto 2
            return
        case (8)
            if (tictac(3,2) /= " ") goto 2
            return
        case (9)
            if (tictac(3,3) /= " ") goto 2
            return
    end select
! return FALSE if the conditions aboved jumped to this statement
    2 CHKPLAY = .FALSE.
    return
end function CHKPLAY

subroutine showBoard(tictac)
    implicit none
!
! Prints the tic-tac-toe board to the screen each time a player makes a move
!
! Argument Definitions --
!   Input Arguments
!       tictac - represents the current state of the board game
    character, dimension(3,3) :: tictac
!
! Local variables
    integer :: i, j, max_rowCol, min_rowCol
!
! Initialize local variables
    min_rowCol = 1
    max_rowCol = 3

! format the spacing in a row of the board
    1 format(" "x,a,x,"|",x,a,x,"|",x,a,x)
!
! Prints the rows and the dividing lines of the tic-tac-toe boards
    do i = min_rowCol, max_rowCol
        write(*,1) (tictac(i,j), j = min_rowCol, max_rowCol)
        if ((i == 1) .OR. (i == 2)) then
            write(*,*) "---+---+---"
        endif
    end do
end subroutine showBoard

logical function same(posOne, posTwo, posThree)
    implicit none
!
! A logical function which tests a row, column or diagonal returns a value of true if all three elements
! are the same (‘X’ or ‘O’); otherwise returns false.
! Return --
!   TRUE or FALSE
!
! Argument Definitions --
!   Input Arguments
!       posOne: represents the character in the first position
!       posTwo: represents the character in the second position
!       posThree: represents the character in the third position
    character :: posOne, posTwo, posThree
!
! Local variable
    logical :: allThreeSame

! Check if all three positions have all X's or O's and assigns TRUE or FALSE to the local variable accordinly
if ((posOne == "X" .AND. posTwo == "X" .AND. posThree == "X") .OR. (posOne == "O" .AND. posTwo == "O" .AND. posThree == "O")) then
    allThreeSame = .TRUE.
else
    allThreeSame = .FALSE.
endif

! Return the value in the local variable to the caller
    same = allThreeSame
    return
end function same

subroutine pickMove(tictac)
    implicit none
!
! Performs the computers move.
!
! Argument Definitions --
!   Input Arguments
!       tictac - represents the current state of the board game
    character, dimension(3,3) :: tictac

! Local variables
    integer :: firstRow_Sum, secondRow_Sum, thirdRow_Sum, firstCol_Sum, secondCol_Sum, thirdCol_Sum, leftDiag, rightDiag, x, o, i, j
    integer :: randOne, randTwo

! Initialize local variables with the proper values
    firstRow_Sum = 0
    secondRow_Sum = 0
    thirdRow_Sum = 0
    firstCol_Sum = 0
    secondCol_Sum = 0
    thirdCol_Sum = 0
    leftDiag = 0
    rightDiag = 0
    x = 1
    o = 4

! Calculate the sum of the first, second, and third rows and columns
    do i=1,3
        do j=1,3
            if(tictac(i,j) == "X") then
                if(i == 1) firstRow_Sum = firstRow_Sum + x
                if(i == 2) secondRow_Sum = secondRow_Sum + x
                if(i == 3) thirdRow_Sum = thirdRow_Sum + x
                if(j == 1) firstCol_Sum = firstCol_Sum + x
                if(j == 2) secondCol_Sum = secondCol_Sum + x
                if(j == 3) thirdCol_Sum = thirdCol_Sum + x
            elseif (tictac(i,j) == "O") then
                if(i == 1) firstRow_Sum = firstRow_Sum + o
                if(i == 2) secondRow_Sum = secondRow_Sum + o
                if(i == 3) thirdRow_Sum = thirdRow_Sum + o
                if(j == 1) firstCol_Sum = firstCol_Sum + o
                if(j == 2) secondCol_Sum = secondCol_Sum + o
                if(j == 3) thirdCol_Sum = thirdCol_Sum + o
            endif
        enddo
    enddo

! Calculate the sum of the diagonal line from left to right
    do i=1,3
        if(tictac(i,i) == "X") leftDiag = leftDiag + x
        if(tictac(i,i) == "O") leftDiag = leftDiag + o
    enddo

! Calculate the sum of the diagonal line from right to left
    j = 3
    do i=1,3
        if(tictac(i,j) == "X") rightDiag = rightDiag + x
        if(tictac(i,j) == "O") rightDiag = rightDiag + o
        j = j - 1
    enddo

! Check if any of the rows, columns, or diagonals and place the computer move 'O' in the appropriate square
    if (firstRow_Sum == 8) then
        do i=1,3
            if (tictac(1,i) == " ") then
                tictac(1,i) = "O"
            endif
        enddo
    elseif (secondRow_Sum == 8) then
        do i=1,3
            if (tictac(2,i) == " ") then
                tictac(2,i) = "O"
            endif
        enddo
    elseif (thirdRow_Sum == 8) then
        do i=1,3
            if (tictac(3,i) == " ") then
                tictac(3,i) = "O"
            endif
        enddo
    elseif (firstCol_Sum == 8) then
        do i=1,3
            if (tictac(i,1) == " ") then
                tictac(i,1) = "O"
            endif
        enddo
    elseif (secondCol_Sum == 8) then
        do i=1,3
            if (tictac(i,2) == " ") then
                tictac(i,2) = "O"
            endif
        enddo
    elseif (thirdCol_Sum == 8) then
        do i=1,3
            if (tictac(i,3) == " ") then
                tictac(i,3) = "O"
            endif
        enddo
    elseif (leftDiag == 8) then
        do i=1,3
            if (tictac(i,i) == " ") then
                tictac(i,i) = "O"
            endif
        enddo
    elseif (rightDiag == 8) then
        j = 3
        do i=1,3
            if(tictac(i,j) == " ") then
                tictac(i,j) = "O"
            endif
            j = j - 1
        enddo
    elseif (firstRow_Sum == 2) then
        do i=1,3
            if(tictac(1,i) == " ") then
                tictac(1,i) = "O"
            endif
        enddo
    elseif (secondRow_Sum == 2) then
        do i=1,3
            if (tictac(2,i) == " ") then
                tictac(2,i) = "O"
            endif
        enddo
    elseif (thirdRow_Sum == 2) then
        do i=1,3
            if (tictac(3,i) == " ") then
                tictac(3,i) = "O"
            endif
        enddo
    elseif (firstCol_Sum == 2) then
        do i=1,3
            if(tictac(i,1) == " ") then
                tictac(i,1) = "O"
            endif
        enddo
    elseif (secondCol_Sum == 2) then
        do i=1,3
            if(tictac(i,2) == " ") then
                tictac(i,2) = "O"
            endif
        enddo
    elseif (thirdCol_Sum == 2) then
        do i=1,3
            if(tictac(i,3) == " ") then
                tictac(i,3) = "O"
            endif
        enddo
    elseif (leftDiag == 2) then
        do i=1,3
            if (tictac(i,i) == " ") then
                tictac(i,i) = "O"
            endif
        enddo
    elseif (rightDiag == 2) then
        j = 3
        do i=1,3
            if(tictac(i,j) == " ") then
                tictac(i,j) = "O"
            endif
            j = j - 1
        enddo
    else
        do
! Generate two random numbers form 1 - 9 to place the computer's move
            randOne = int(rand(0)*9)+1
            randTwo = int(rand(0)*9)+1
            if (tictac(randOne, randTwo) == " ") then
                tictac(randOne, randTwo) = "O"
                return
            endif
        enddo
    endif
end subroutine pickMove

subroutine CHKOVR(tictac)
    implicit none
!
! Check if Tic-Tac-Toe game is over and determine a winner (if any), or draw
!
! Argument definitions --
!   Input Arguments
!       tictac - represents the current state of the board game
    character, dimension(3,3) :: tictac
!
! Functions used
    logical :: same
!
! Local variables
    character :: winner
    integer :: ir, ic
!
! Initialize local variable winner
    winner = " "
!
! Check for a winner
! Check rows for winner
    do ir=1, 3
        if (same(tictac(ir,1), tictac(ir,2), tictac(ir,3))) then
            winner = tictac(ir,1)
            write(*,*)'Winner is ', winner,'! Game over!!'
            stop
        endif
    enddo
! Check column for winner
    do ic=1, 3
        if (same(tictac(1,ic), tictac(2,ic), tictac(3,ic))) then
            winner = tictac(1,ic)
            write(*,*)'Winner is ', winner,'! Game over!!'
            stop
        endif
    enddo
! Check diagonal for winner
    if ((same(tictac(1,1), tictac(2,2), tictac(3,3))) .OR. (same(tictac(1,3), tictac(2,2), tictac(3,1)))) then
        winner = tictac(2,2)
        write(*,*)'Winner is ', winner,'! Game over!!'
        stop
    endif
! No winner at all. See if game is a draw
! Check each row for an empty space
    do ir = 1,3
        do ic = 1,3
            if (tictac(ir,ic) == " ") then
                return
            endif
        enddo
    enddo
!
! No blank found, game is a draw
    write(*,*)'Its a Draw! Game over!!'
    stop
end subroutine CHKOVR
