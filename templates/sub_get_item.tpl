
	my $data = sql ('__TYPE__');

#	$data -> {no_del} ||= 1 if $data -> {id_user} != $_USER -> {id};

	$_REQUEST {__read_only} ||= !($_REQUEST {__edit} || $data -> {fake} > 0);

#	add_vocabularies ($data, 
#		voc_foo => {order => "id", filter => "id=$$data{id___TYPE__}"}
#	);

	$_REQUEST {first} += 0;
	
	$data -> {clones} = sql (__TYPE__ => [
		['label LIKE', substr ($data -> {label}, 0, ($_REQUEST {first} ||= 10)) . '%'],
	]);

#	sql ($data, __TYPE___log => [
#	
#		[ id___TYPE__ => $data -> {id} ],
#		
#		[ ORDER       => ['id DESC'] ],
#		[ LIMIT       => [0 + $_REQUEST {start}, $conf -> {portion}]],
#		
#	], 'voc_something(*)', 'log(dt)', 'users');

	return $data;
