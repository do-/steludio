
	$item = sql_select_hash ('__TYPE__');

	set_def ($_REQUEST [__read_only], !($_REQUEST [__edit] || $item [fake] > 0));

#	add_vocabularies ($item, 
#		voc_foo => array (order => "id", filter => "id=$data[id___TYPE__]")
#	);

	$item -> {path} = [
		array (type => '__TYPE__', name => '???'),
		array (type => '__TYPE__', name => $item [label], id => $item [id]),
	];

	unless ($_REQUEST [first]) {
		$_REQUEST [first] = strlen ($item [label]);
		$_REQUEST [first] = 4 if $_REQUEST [first] > 4;
	}

	$item [clones] = sql_select_all (<<<EOS
		SELECT
			__TYPE__.*
		FROM
			__TYPE__
		WHERE
			LEFT(__TYPE__.label, $_REQUEST{first}) = LEFT(?, $_REQUEST{first})
		ORDER BY
			__TYPE__.label
EOS
, $item [label], array (fake => '__TYPE__'));

	return $item;