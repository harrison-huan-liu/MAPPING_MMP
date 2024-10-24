function HCP_MMP_binary_GLM_on_CFC(task_start,task_end,frequency_num,node_size)
% binary_code_node = binary_code(4,4,3,360,1);
current_file = mfilename('fullpath');
[current_path, ~, ~] = fileparts(current_file);
[current_path, ~, ~] = fileparts(current_path);
% frequency_num=3; 
% node_size=360; 
common_or_difference=1;
for task_i = task_start:task_end % task_start = 2; task_end = 7;
    for task_j = 1:task_i-1
        if common_or_difference == 0
            filename_pvalue = [current_path, '/to_DFYANG/HCP360/linearregression_pvalue_', num2str(task_i), '_', num2str(task_j), '.mat'];
            filename_estimate = [current_path, '/to_DFYANG/HCP360/linearregression_estimate_', num2str(task_i), '_', num2str(task_j), '.mat'];
        else
            filename_pvalue = [current_path, '/to_DFYANG/HCP360/linearregression_pvalue_ondifference_', num2str(task_i), '_', num2str(task_j), '.mat'];
            filename_estimate = [current_path, '/to_DFYANG/HCP360/linearregression_estimate_ondifference_', num2str(task_i), '_', num2str(task_j), '.mat'];
        end
        % filename = [current_path, '/data/to_DFYANG/ttest_value_', num2str(task_i), '_', num2str(task_j), '.mat'];
        load(filename_pvalue);
        load(filename_estimate);
        % imagesc(linearregression_pvalue)
        % first. only node, no edge; (binary_code_node)
        % second. only node, reduce the number of boxes; (binary_code_node_reduce)
        binary_code_node = zeros(node_size, node_size);
        binary_code_node_reduce = zeros(node_size, node_size);
        for i = 1:frequency_num
            freq_i = frequency_num + 1 - i;
            for crossfrequency = 1:freq_i
                for node_i = (crossfrequency-1)*node_size+1:crossfrequency*node_size
                    transform_i = node_i-(crossfrequency-1)*node_size;
                    if linearregression_pvalue(node_i+node_size*(i-1), node_i)<10e-12
                        temp = sum(1:crossfrequency+i-1)-i;
                        binary_code_node(transform_i, transform_i) = binary_code_node(transform_i, transform_i) + 2^(temp);
                    end
                end
            end
        end
        for j = 1:frequency_num
            for node_i = 1:node_size
                if linearregression_pvalue(node_i+(j-1)*node_size,node_i)<10e-8
                    temp = 2^(j-1);
                    binary_code_node_reduce(node_i,node_i) = binary_code_node_reduce(node_i,node_i) + temp;
                end
            end
        end
        Node_HCP_workbench = diag(binary_code_node);
        Node_HCP_workbench_savepath = [current_path, '/Mapping_MMP/intermediate_binary_result/Node_HCPworkbench_binary_', num2str(task_i), '_', num2str(task_j), '.txt'];
        writematrix(Node_HCP_workbench,Node_HCP_workbench_savepath,'Delimiter','tab')
        % create_surface_func_gii(binary_code_node,task_i,task_j,current_path)
        Node_reduce_HCP_workbench = diag(binary_code_node_reduce);
        Node_reduce_HCP_workbench_savepath = [current_path, '/Mapping_MMP/intermediate_binary_result/Node_reduce_HCPworkbench_binary_', num2str(task_i), '_', num2str(task_j), '.txt'];
        writematrix(Node_reduce_HCP_workbench,Node_reduce_HCP_workbench_savepath,'Delimiter','tab')
        % create_surface_func_gii(binary_code_node_reduce,task_i,task_j,current_path)
    end
end
end


%% create the gifti file by assigning arbitrary values to surface parcels (MMP parcels)
%% see on https://mvpa.blogspot.com/2017/11/tutorial-assigning-arbitrary-values-to.html
%% the gifti library still don't work, there are some problem on the function read_gifti_file.
% out = repelem(0, 180);  
% out(1) = 1;  
% out(10) = 1;  
% out(15) = 1;  
function create_surface_func_gii(binary_code_node,task_i,task_j,current_path)
out = diag(binary_code_node);
parcelNums_path = [current_path, '/Mapping_MMP/plot_data/parcelNums_', task_i, '_', task_j, '.csv'];
csvwrite(parcelNums_path, out)

% workbench commond
system(['wb_command -cifti-separate ' ...
    current_path, '/Mapping_MMP/Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii ' ...
    'COLUMN -metric CORTEX_LEFT ' , current_path, '/Mapping_MMP/plot_data/mmpL_', task_i, '_', task_j, '.func.gii'])
system(['wb_command -cifti-separate ' ...
    current_path, '/Mapping_MMP/Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii ' ...
    'COLUMN -metric CORTEX_RIGHT ' , current_path, '/Mapping_MMP/plot_data/mmpR_', task_i, '_', task_j, '.func.gii'])

addpath 'C:\Program Files\MATLAB\R2023a\toolbox\gifti';  % so matlab can find the library
Lfuncgii_path = [current_path, '/Mapping_MMP/plot_data/mmpL_', task_i, '_', task_j, '.func.gii'];
Rfuncgii_path = [current_path, '/Mapping_MMP/plot_data/mmpR_', task_i, '_', task_j, '.func.gii'];
mmpL = gifti(Lfuncgii_path);  % load the left side gifti MMP atlas  
mmpR = gifti(Rfuncgii_path);  % and the right side MMP atlas  
newvals = csvread(parcelNums_path);  % 180 integers; new value for each parcel  
Lout = mmpL;  % output gifti  
Lout.cdata(:,1) = repelem(0, size(mmpL.cdata,1));  % replace the values with zeros  
Rout = mmpR;  
Rout.cdata(:,1) = repelem(0, size(mmpR.cdata,1));   
for i=1:180  % i = 1;  
    inds = find(mmpR.cdata == i);  % find vertices for parcel i  
    Rout.cdata(inds,1) = newvals(i);  % put the new value in for this parcel's vertices  
    inds = find(mmpL.cdata == (i+180)); % in MMP, left hemisphere vertices are 181:360  
    Lout.cdata(inds,1) = newvals(i);    
end
save_plotLfuncgii_path = [current_path, '/Mapping_MMP/figure/plotL_', task_i, '_', task_j, '.func.gii'];
save_plotRfuncgii_path = [current_path, '/Mapping_MMP/figure/plotR_', task_i, '_', task_j, '.func.gii'];
save(Lout,save_plotLfuncgii_path,'Base64Binary');  % save the gifti  
save(Rout,save_plotRfuncgii_path,'Base64Binary');  
end
