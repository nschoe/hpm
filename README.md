hpm - Haskell Password Manager
==============================

hpm is my humble attempt at creating a simple, lightweight password
management application.
hpm is vanilla : there is no fancy add-ons, useless features,
etc. Namely, hpm won't sync with external *data-sucking* servers nor
will it ask you to *Like it* or *Tweet it* or whatever ; sometimes
things should do what they were intended to do, hpm is a password
manager, so it will store and manage your passwords.
You can start using really long and hard to guess passwords for
everything, as it will hpm's job to remember them for you.

Changelog
=========

- March 26, 2014
    - Project initialization. I am going back to Haskell, and this is a
recover project. Looks awesome.
    - Verification of existing ~/.hpm/ folder and create it if non existing
    - Asking for master password and checking if a corresponding entry
book is associated

- March 28, 2014
    - Initialization of new library books is working : several books
can be created with different master passwords, and hpm warns the user
if he is trying to create a book with the same master password
    - Implemented reset of a new entry book : deletes all passwords in
the entry book associated with his master password

TODO
====
- Hide the character typing in the terminal when asking the user its
master password
- Implement hashing of master password
- Implement encryption of library book and entry books
