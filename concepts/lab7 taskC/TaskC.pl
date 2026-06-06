:- dynamic book/4.
:- dynamic borrowed/4.
% These dynamic predicates allow us to add and remove predicates depicting facts in these categories during runtime, rather than needing to write them beforehand or in a file. book/4 represents the books in the library, meaning books that get borrowed are 'transfered' over to the borrowed-books facts, its own dynamic predicate representing the books in a 'borrowed' state.  


add_book(Title, Author, Year, Genre):-
    not(is_available(Title, Author, Year, Genre)),
    not(borrowed(Title, Author, Year, Genre)),
    assertz(book(Title, Author, Year, Genre)).
% This one creates the book/4 predicate with the given attributes, but also ensures the book doesn't already exist in either the library or the list of borrowed books.


remove_book(Title, Author, Year, Genre):-
    is_available(Title, Author, Year, Genre),
    retract(book(Title,Author, Year, Genre)).
% This deletes the book with the given attributes, but only if it's available (is a book, and is not borrowed).

is_available(Title, Author, Year, Genre):-
    book(Title, Author, Year, Genre),
    not(borrowed(Title,Author,Year,Genre)).
% This checks that the given books exists and is in the library (and is not borrowed).

borrow_book(Title,Author,Year,Genre):-
    is_available(Title, Author, Year, Genre),
    assertz(borrowed(Title,Author,Year,Genre)),
    retract(book(Title,Author,Year,Genre)).
% This checks that the given book is available, and then transfers it from 'book' to 'borrowed'.

return_book(Title,Author,Year,Genre):-
    retract(borrowed(Title,Author,Year,Genre)),
    add_book(Title,Author,Year,Genre).
% This removes the book from 'borrowed' (thus failing if it's not found), and adds it to 'book'

% The following retrieve all books that meat the given criteria, whether they are in the library or are borrowed. 
find_by_author(Auth,B):-
    recommend_by_author(Auth,BR),
    findall(Title, borrowed(Title,Auth,_,_),BB),
    append(BR, BB, B).

find_by_genre(Genre,B):-
    recommend_by_genre(Genre,BR),
    findall(Title, borrowed(Title,_,_,Genre),BB),
    append(BR, BB, B).

find_by_year(Year,B):-
    findall(Title, book(Title,_,Year,_),BR),
    findall(Title, borrowed(Title,_,Year,_),BB),
    append(BR, BB, B).

% These are like the 'find_by' predicates, but they only search for books within the library (and are thus not currently borrowed)
recommend_by_author(Auth,B):-
    findall(Title, book(Title,Auth,_,_),B).

recommend_by_genre(Genre,B):-
    findall(Title, book(Title,_,_,Genre),B).



