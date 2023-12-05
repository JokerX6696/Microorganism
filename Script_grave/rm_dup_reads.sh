ls /storge1/automation/projects/DNA/dna/2bRAD-M/2023/10/DZOE2023091937-b1/Basic/Clean_data/*NC*/*gz|while read i
do
    cat $i
done > All_NC.fq.gz

mkdir cleandata
cd cleandata
/data/software/seqkit/0.13.2/seqkit rmdup ../All_NC.fq.gz -o rm_dup_NC.fq.gz  # 有重复 reads 名称 报错
ls /storge1/automation/projects/DNA/dna/2bRAD-M/2023/10/DZOE2023091937-b1/Basic/Clean_data/*/*gz|grep -v NC|while read i
do
    prefix=$(echo $i|awk -F "/" '{print $(NF-1)}')
    echo $i $prefix
    /home/liujiang/software/install/mamba/envs/python.3.11.4/bin/python3 \
    /home/liujiang/scripts/2bM/2BDecon.py \
    -s $i \
    -n /storge1/automation/projects/DNA/dna/2bRAD-M/2023/10/DZOE2023091937-b1/Suppl_1/cleandata/rm_dup_NC.fq.gz \
    -p $prefix
    if [ $? eq 0 ];then
        echo $i finish in `data` >> log
    fi

done



 