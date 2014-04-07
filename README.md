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
everything, as it will hpm's job to remember them for you !

Usage
=====

Note : 
- a service design the entity to which your (user, password) pair is
associated, put whatever you want in it : website name, program name,
etc
- a user is typically the id used to identify, sometimes it is your
username, sometimes it is your email address.
- a password is, well ... your password. As you won't have neither to
remember nor to type it, I suggest you start using *real* passwords
like 'dhh*&4*4"}{++)Jd$£fn' and so on. You can go to
howsecureismypassword.net to have some hints about your password
strength (just be aware that the computing power they estimate is the
one of a single desktop PC, not a multi-million server, so you'd
better divide the number by 1e6 to have a more accurate description)


Here's how to use hpm :
-----------------------

- create an new entry book and a master password : hpm -i, --init (a
master password is the only password you will have to remember to
manage your passwords, a book is the (encrypted) list of passwords)
- adding a new password : hpm -a, --add <service> <user> (you will be
  prompted for the password you want to add to that service)
- extracting a password for a service : hpm -e, --extract <service>
[<user>] (can be omitted if only one user)
- deleting a stored password : hpm -d, --delete <service> <user>
- listing your services for which you have a stored password : hpm -l, --list

Changelog
=========
- April 7, 2014
    - Added support for several entries for the same service
    - Deletion now requires a service and a user

- April 2, 2014
    - Removed SHA512-hasing of master passwords as it throws some
problems for now. It doesn't really matter much : the library file is
already encrypted, so the master passwords never appear in plain text
    - Implemented hiding of characters when typing passwords
    - Now the password is stored in the clipboard when extracted, not
printed on screen anymore

- March 31, 2014
    - Implemeted SHA512-hashing of master passwords to store the hash
in the book library

- March 29, 2014
    - Adding a new entry in one's book is working with the limitation
of one password entry per service (this is a choice : so that we can
extract a password with just **hpm -e service** rather than
**hpm -e service user** ; the tool is designed to be *quick* to use)
    - Listing and counting stored entries working
    - Deleting an entry working (deletion is quiet : it won't warn
your nor will it fail is you try to delete a non-existing entry, in
this case, your book will jsut stay the same, again for speed of use)
    - Extracting an entry is working

- March 28, 2014
    - Initialization of new library books is working : several books
can be created with different master passwords, and hpm warns the user
if he is trying to create a book with the same master password
    - Implemented reset of a new entry book : deletes all passwords in
the entry book associated with his master password

- March 26, 2014
    - Project initialization. I am going back to Haskell, and this is a
recover project. Looks awesome.
    - Verification of existing ~/.hpm/ folder and create it if non existing
    - Asking for master password and checking if a corresponding entry
book is associated

TODO
====
- Rewrite 'initiate' function to use 'withLibrary' rather than calling
'withLibrary initiate' in the main function
- Rewrite the code in a more elegant fashion : capture the redundant
code between 'list', 'delete' and 'extract' for instance or the
awkward 'Right _' and 'Left _' cases in 'extract' function : I'm
pretty sure it sounds Monad-ish behavior.
- Create a new thread that waits 10 seconds and then erase the
contents of the clipboard when extracting a password. So that the
password doesn't remain in the clipboard.
