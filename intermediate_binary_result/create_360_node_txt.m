function [] = create_360_node_txt(node_num)
for i=1:node_num
    node_txt = zeros(360,1);
    node_txt(i) = 1;
    node_txt_filename = ['node_txt_', num2str(i), '.txt'];
    save(node_txt_filename,'node_txt','-ascii')
end
end