#! /bin/bash
while IFS=, read -r col1 col2
do
	initial="$(echo $col2 | head -c 1)"
	last="$(echo -e "${col1}" | tr -d '[:space:]')"
	lower_initial="$(echo $initial | tr '[:upper:]' '[:lower:]')"
	lower_last="$(echo $last | tr '[:upper:]' '[:lower:]')"
	user="$(echo $lower_initial$lower_last)"
	echo $user
	useradd -m $user
	echo $user:$user | chpasswd
	mkdir /home/$user/Lab1
	mkdir /home/$user/Lab2
	mkdir /home/$user/Lab3
	cp -rf /home/lib/Lab1/ /home/$user/
	cp -rf /home/lib/Lab2/ /home/$user/
	cp -rf /home/lib/Lab3/ /home/$user/
done < /home/roster.csv