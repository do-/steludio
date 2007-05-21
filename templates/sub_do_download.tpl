	sql_download_file ({
		name => 'file',
		dir => 'upload/images',
		table => '__TYPE__',
		file_name_column => 'file_name',
		size_column => 'file_size',
		type_column => 'file_type',
		path_column => 'file_path',
		no_force_download => 0,
	});
