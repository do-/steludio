
	my $item = sql_select_hash ('__TYPE__');

	$_REQUEST {__read_only} ||= !($_REQUEST {__edit} || $item -> {fake} > 0);

#	add_vocabularies ($item, 
#		voc_foo => {order => "id", filter => "id=$$data{id___TYPE__}"}
#	);

	unless ($_REQUEST {first}) {
		$_REQUEST {first} = length $item -> {label};
		$_REQUEST {first} = 4 if $_REQUEST {first} > 4;
	}

	$item -> {clones} = sql_select_all (<<EOS, $item -> {label}, {fake => '__TYPE__'});
		SELECT
			__TYPE__.*
		FROM
			__TYPE__
		WHERE
			LEFT(__TYPE__.label, $_REQUEST{first}) = LEFT(?, $_REQUEST{first})
		ORDER BY
			__TYPE__.label
EOS

	return $item;
